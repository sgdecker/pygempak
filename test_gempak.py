import unittest
import numpy as np
import gempak as gp

gemfile = 'nam211.gem'

# This function from:
#   https://github.com/larsbutler/oq-engine/blob/master/tests/utils/helpers.py
def assertDeepAlmostEqual(test_case, expected, actual, *args, **kwargs):
    """
    Assert that two complex structures have almost equal contents.

    Compares lists, dicts and tuples recursively. Checks numeric values
    using test_case's :py:meth:`unittest.TestCase.assertAlmostEqual` and
    checks all other values with :py:meth:`unittest.TestCase.assertEqual`.
    Accepts additional positional and keyword arguments and pass those
    intact to assertAlmostEqual() (that's how you specify comparison
    precision).

    :param test_case: TestCase object on which we can call all of the basic
    'assert' methods.
    :type test_case: :py:class:`unittest.TestCase` object
    """
    is_root = not '__trace' in kwargs
    trace = kwargs.pop('__trace', 'ROOT')
    try:
        if isinstance(expected, (int, float, long, complex)):
            test_case.assertAlmostEqual(expected, actual, *args, **kwargs)
        elif isinstance(expected, (list, tuple, np.ndarray)):
            test_case.assertEqual(len(expected), len(actual))
            for index in xrange(len(expected)):
                v1, v2 = expected[index], actual[index]
                assertDeepAlmostEqual(test_case, v1, v2,
                                      __trace=repr(index), *args, **kwargs)
        elif isinstance(expected, dict):
            test_case.assertEqual(set(expected), set(actual))
            for key in expected:
                assertDeepAlmostEqual(test_case, expected[key], actual[key],
                                      __trace=repr(key), *args, **kwargs)
        else:
            test_case.assertEqual(expected, actual)
    except AssertionError as exc:
        exc.__dict__.setdefault('traces', []).append(trace)
        if is_root:
            trace = ' -> '.join(reversed(exc.traces))
            exc = AssertionError("%s\nTRACE: %s" % (exc.message, trace))
        raise exc


class GempakTestCase(unittest.TestCase):
    def setUp(self):
        self.data = gp.Dataset(gemfile)

    def tearDown(self):
        self.data = None

    def test_ang(self):
        self.assertEqual(self.data.ang[0], 25, 'test_ang: ang[0] incorrect')
        self.assertEqual(self.data.ang[1], -95, 'test_ang: ang[1] incorrect')
        self.assertEqual(self.data.ang[2], 25, 'test_ang: ang[2] incorrect')

    def test_datainfo(self):
        self.assertEqual(self.data.datainfo[10], {'gdattim': 
                                                  ['150610/1200F000', ''],
                                                  'gfunc': 'VREL',
                                                  'glevel': [10, -1],
                                                  'gvcord': 'HGHT'}, 
                         'test_datainfo: datainfo[10] incorrect')
        
    def test_gemfile(self):
        self.assertEqual(self.data.gemfile, 'nam211.gem', 'gemfile incorrect')

    def test_lllat(self):
        self.assertAlmostEqual(self.data.lllat, 12.1899995803833,
                               'lllat incorrect')

    def test_lllon(self):
        self.assertAlmostEqual(self.data.lllon, -133.45899963378906,
                               'lllon incorrect')

    def test_max_grids(self):
        self.assertEqual(self.data.max_grids, 5000, 'max_grids incorrect')

    def test_num_grids(self):
        self.assertEqual(self.data.num_grids, 1847, 'num_grids incorrect')

    def test_nx(self):
        self.assertEqual(self.data.nx, 93, 'nx incorrect')

    def test_ny(self):
        self.assertEqual(self.data.ny, 65, 'ny incorrect')
    
    def test_proj(self):
        self.assertEqual(self.data.proj, 'LCC', 'proj incorrect')
    
    def test_urlat(self):
        self.assertAlmostEqual(self.data.urlat, 57.289485931396484,
                               'urlat incorrect')

    def test_urlon(self):
        self.assertAlmostEqual(self.data.urlon, -49.38454818725586,
                               'urlon incorrect')

    def test_grid_from_num(self):
        self.assertAlmostEqual(self.data.grid_from_num(17)[10,20], 291.57324,
                               places=4, msg='grid_from_num incorrect')

    def test_grid_from_dict(self):
        d = {'gdattim': ['150610/1200F000', ''], 'gfunc': 'VREL',
             'glevel': [700, -1], 'gvcord': 'PRES'}
        self.assertAlmostEqual(self.data.grid_from_dict(d)[40,43], 3.6323986,
                               places=4, msg='grid_from_dict incorrect')

    def test_map_for_dataset(self):
        m = gp.map_for_dataset(self.data)
        params = {'R': 6370997.0, 'lat_1': 25.0, 'lat_2': 25.0, 'lon_0': -95.0,
                  'proj': 'lcc', 'units': 'm', 'x_0': 4225953.082964148,
                  'y_0': -2035294.1646823965}
        assertDeepAlmostEqual(self, m.projparams, params,
                              'map_for_dataset incorrect')

suite = unittest.TestLoader().loadTestsFromTestCase(GempakTestCase)
unittest.TextTestRunner(verbosity=2).run(suite)
