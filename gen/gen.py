import os
import sys
import yaml
import argparse
import collections
from jinja2 import FileSystemLoader, Environment


def app_type(x):
    """
    Function to check --app command line argument for supported applications
    """
    lst_app = ['cesm', 'nems', 'hafs']
    if x.lower() in lst_app:
        return x
    else:
        xstr = '['+', '.join([str(elem) for elem in lst_app])+']'
        raise argparse.ArgumentTypeError('{} are expected for application \
              name but {} given!'.format(xstr, x))


def do_render(filename, dict, app):
    file_loader = FileSystemLoader('templates')
    env = Environment(loader=file_loader, trim_blocks=True, lstrip_blocks=True)
    env.filters['str_format'] = str_format
    template = env.get_template(filename)
    template_parsed = template.render(dict)
    do_write(template_parsed, app)


def do_write(template, app):
    with open('esmFldsExchange_{}_mod.F90'.format(app), 'w') as fout:
        fout.write(template)


def parse(data):
    # define global dictonary to store information to pass template
    global_dict = {}

    # temporary variables
    comps = []
    target_comps = []
    conns = []
    map_types = []

    # parse mapping definition
    mapping = collections.defaultdict(list)
    for field in data['mapping']:
        fdmatch = True
        mapnorm = 'unset'
        mapfile = 'unset'
        maptype = 'unset'
        mrgtype = 'unset'
        mrgsrc = 'unset'
        mrgwgt = 'unset'

        for item in list(field.values())[0]:
            if 'destination' in item.keys():
                # parse source and destination fields and their components
                dst = item['destination'].split('.')
                src = list(field.keys())[0].split('.')
                # check field for merging information
                hasMerge = next((x for x in data['merging'] if item['destination'] in x.keys()), None)
                if hasMerge is not None:
                    for mrg_item in hasMerge[item['destination']]:
                        # parse merging information
                        if 'type' in mrg_item:
                            mrgtype = mrg_item['type']
                        if 'sources' in mrg_item:
                            mrgsrc = mrg_item['sources']
                        if 'weights' in mrg_item:
                            mrgwgt = mrg_item['weights']
                # parse the list of active component and the connection
                # among those components
                comps.append(dst[0])
                comps.append(src[0])
                target_comps.append(dst[0])
                if 'med' not in dst[0] and 'med' not in src[0]:
                    conns.append('{}2{}'.format(src[0], dst[0]))
            # get mapping type
            if 'method' in item.keys():
                maptype = item['method']
                map_types.append(maptype)
            # get normalization type
            if 'normalization' in item.keys():
                mapnorm = item['normalization']
            # get pointer of static mapping file
            if 'map_file' in item.keys():
                mapfile = item['map_file']
            # check target variable name is different than the source one
            if src[1] not in dst[1]:
                fdmatch = False

        mapping[(dst[0], src[0], maptype, mapnorm, mrgtype, mrgwgt, fdmatch)].append(src+dst+[maptype, mapnorm, mapfile])
        for k in mapping.keys():
            print(k)

    # sort mapping dictionary based on target component
    mapping = dict(sorted(mapping.items()))

    # sort target components
    target_comps = list(set(target_comps))
    target_comps.sort()

    # fill global dictionary
    global_dict['mapping'] = mapping
    global_dict['comps'] = list(set(comps))
    global_dict['target_comps'] = target_comps

    conns = list(set(conns))
    conns.sort()
    global_dict['conns'] = conns

    global_dict['map_types'] = list(set(map_types))

    return global_dict


def read_yaml(ifile):
    """
    Opens and read YAML file
    """
    with open(ifile) as f:
        data = yaml.safe_load(f)
    return data


def str_format(str, len):
    return str.ljust(len)


def main(argv):
    """
    Main application that creates application specific esmFldsExchange
    Fortran module file
    """
    # defaults
    app = 'nems'
    debug = False

    # read input arguments
    parser = argparse.ArgumentParser()
    parser.add_argument('--app', help='Application name',
                        required=True, type=app_type)
    parser.add_argument('--debug', help='Enable debug mode, defaults to False',
                        required=False, action='store_true')
    args = parser.parse_args()

    if args.app:
        app = args.app
    if args.debug:
        debug = args.debug

    # read yaml file
    data = read_yaml(os.path.join('data', '{}.yaml'.format(app)))

    # parse the data
    global_dict = parse(data)

    # add app information to dictionary
    global_dict['app'] = app
    if debug:
        print(global_dict)

    # render template and write to file
    do_render('esmFldsExchange_mod.jinja2', global_dict, app)


if __name__ == "__main__":
    main(sys.argv[1:])
