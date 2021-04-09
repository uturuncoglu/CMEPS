import argparse
import collections
import sys
import yaml
from jinja2 import FileSystemLoader, Environment

def app_type(x):
    lst_app = ['cesm', 'nems', 'hafs']
    if x.lower() in lst_app:
        return x
    else:
        xstr = '['+', '.join([str(elem) for elem in lst_app])+']'
        raise argparse.ArgumentTypeError('{} are expected for application name but {} given!'.format(xstr, x))

def active_comps(data):
    comps = []
    for d in ['src', 'dst']:
        for k, v in data['map'].items():
            comps.append(v['{}_component'.format(d)])
    comps = list(set(comps))
    comps.sort()
    return comps

def active_conns(data):
    conns = []
    for k, v in data['map'].items():
        conns.append('{}2{}'.format(v['src_component'], v['dst_component']))
    conns = list(set(conns))
    conns.sort()
    return conns

def do_render(filename, dict):
    file_loader = FileSystemLoader('templates')
    env = Environment(loader=file_loader, trim_blocks=True, lstrip_blocks=True)
    env.filters['str_format'] = str_format
    template = env.get_template(filename)
    template_parsed = template.render(dict)
    do_write(template_parsed)

def do_write(template):
    with open('esmFldsExchange_mod.F90', 'w') as fout:
        fout.write(template)

def read_yaml(ifile):
    with open(ifile) as f:
         data = yaml.safe_load(f)
    return data

def get_flds(data, comps):
    field_dict = collections.defaultdict(dict)
    # add entries from map
    for k, v in data['map'].items():
        if v['dst_component'] in comps:
            key = '{}2{}'.format(v['src_component'],v['dst_component'])
            if k != field_dict[key]:
                field_dict[key][k] = k
    # add entries from mrg
    for k, v in data['mrg'].items():
        if v['field']:
            key = '{}2{}'.format(v['mrg_from'],v['mrg_to'])
            if k != v['field'] and v['field'] not in field_dict[key]:
                field_dict[key][k] = v['field']
    # sort by key len
    field_dict_sorted = collections.defaultdict(dict)
    for k, v in field_dict.items():
        for s in sorted(v, key=len, reverse=True):
            field_dict_sorted[k][s] = v[s]
    return dict(field_dict_sorted)

def get_maps(data, comps):
    # create dictionary
    map_dict = collections.defaultdict(lambda: collections.defaultdict(list))
    # group fields based on coupling direction, map_type, map_norm and map_file
    for k, v in data['map'].items():
        key = '{}2{}'.format(v['src_component'],v['dst_component'])
        # it handles only the cases that field and mrg_fld are same
        # it allows to define merge call inside of the map loop
        # the spacial cases will be handled seperately by get_mrgs
        merge = False
        if k in data['mrg']:
            if data['mrg'][k]['field'] == data['mrg'][k]['mrg_fld']:
                merge = True
        # create dictionary for map definitions
        map_dict[key][(v['map_type'], v['map_norm'], v['map_file'], merge)].append(k)
    return dict(map_dict)

def get_map_types(data):
    map_types = []
    for k, v in data['map'].items():
        map_types.append(v['map_type'])
    map_types = list(set(map_types))
    map_types.sort()
    return map_types

def get_mrgs(data, comps):
    mrg_dict = collections.defaultdict(dict)
    # group fields based on coupling direction, map_type, map_norm and map_file
    for k, v in data['mrg'].items():
        if data['mrg'][k]['field'] is None:
            data['mrg'][k]['field'] = k
        if data['mrg'][k]['mrg_fld'] is None:
            data['mrg'][k]['mrg_fld'] = k
        if data['mrg'][k]['field'] != data['mrg'][k]['mrg_fld']:
            key = '{}2{}'.format(v['mrg_from'],v['mrg_to'])
            mrg_dict['{}:{}'.format(key,k)] = v
    return dict(mrg_dict)

def str_format(str, len):
    return str.ljust(len)

def main(argv):
    """
    Main application that creates application specific esmFldsExchange Fortran module file
    """
    # defaults
    app = 'hafs'
    debug = False

    # read input arguments
    parser = argparse.ArgumentParser()
    parser.add_argument('--app', help='Application name', required=True, type=app_type)
    parser.add_argument('--debug', help='Enable debug mode, defaults to False', required=False, action='store_true')
    args = parser.parse_args()

    if args.app:
        app = args.app
    if args.debug:
        debug = args.debug

    # create empty dictionary for template
    global_dict = {}

    # read yaml file
    data = read_yaml('{}.yaml'.format(app))

    # active model components and connections
    comps = active_comps(data)
    conns = active_conns(data)
    global_dict['comps'] = comps
    global_dict['conns'] = conns

    # field list required for each component
    flds = get_flds(data, comps)
    global_dict['fields'] = flds

    # mapping information required for init phase
    maps = get_maps(data, comps)
    global_dict['maps'] = maps
    map_types = get_map_types(data)
    global_dict['map_types'] = map_types

    # merging information required for init phase
    mrgs = get_mrgs(data, comps)
    global_dict['mrgs'] = mrgs

    # print data used to render jinja2 template
    if debug:
        for k, v in global_dict.items():
            print(k, v)

    # render template and write to file
    do_render('esmFldsExchange_mod.jinja2', global_dict)

if __name__== "__main__":
	main(sys.argv[1:])
