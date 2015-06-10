import unittest
import os
import numpy as np
import gempakf as gpf

gemfile = 'nam211.gem'

class GempakTestCase(unittest.TestCase):
    def setUp(self):
        self.n = gpf.gemread.get_num_grids(gemfile)
        self.nx, self.ny = gpf.gemread.get_grid_dims(gemfile)
        self.gdattm = np.zeros((20,self.n,2), np.int8, 'F')
        self.level = np.zeros((self.n,2), np.int32, 'F')
        self.ivcord = np.zeros(self.n, np.int32)
        self.vcord = np.zeros((4,self.n), np.int8, 'F')
        self.parm = np.zeros((12,self.n), np.int8, 'F')

    def tearDown(self):
        self.n = None
        self.nx = None
        self.ny = None
        self.gdattm = None
        self.level = None
        self.ivcord = None
        self.vcord = None
        self.parm = None

    def test_get_max_grids(self):
        self.assertEqual(gpf.gemread.get_max_grids(gemfile), 5000,
                         'get_max_grids incorrect')

    def test_get_num_grids(self):
        self.assertEqual(self.n, 1847,
                         'get_num_grids incorrect')

    def test_get_grid_dims(self):
        self.assertEqual((self.nx, self.ny), (93, 65),
                         'get_grid_dims incorrect')

    def test_ggi(self):
        max_grids, num_grids, nx, ny, proj, ang, lllat, lllon, urlat, urlon = \
            gpf.gemread.ggi(gemfile, self.gdattm, self.level, self.ivcord,
                            self.vcord, self.parm)
        self.assertEqual(max_grids, 5000, 'ggi: max_grids incorrect')
        self.assertEqual(num_grids, 1847, 'ggi: num_grids incorrect')
        self.assertEqual(nx, 93, 'ggi: nx incorrect')
        self.assertEqual(ny, 65, 'ggi: ny incorrect')
        self.assertEqual(proj, 'LCC                 ', 'ggi: proj incorrect')
        self.assertEqual(ang[0], 25, 'ggi: ang[0] incorrect')
        self.assertEqual(ang[1], -95, 'ggi: ang[1] incorrect')
        self.assertEqual(ang[2], 25, 'ggi: ang[2] incorrect')
        self.assertAlmostEqual(lllat, 12.1899995803833, 'ggi: lllat incorrect')
        self.assertAlmostEqual(lllon, -133.45899963378906,
                               'ggi: lllon incorrect')
        self.assertAlmostEqual(urlat, 57.289485931396484,
                               'ggi: urlat incorrect')
        self.assertAlmostEqual(urlon, -49.38454818725586,
                               'ggi: urlon incorrect')

    def test_read_grid(self):
        grid = np.zeros((self.nx,self.ny), np.float32, 'F')
        gpf.gemread.read_grid(gemfile, '150610/1200F000', '', 850, -1, 'PRES',
                              'TMPK', grid)
        self.assertAlmostEqual(grid[10,20], 285.79053, places=4,
                               msg='read_grid incorrect')
        self.assertAlmostEqual(grid[20,10], 287.54053, places=4,
                               msg='read_grid incorrect')
        self.assertAlmostEqual(grid[40,41], 290.29053, places=4,
                               msg='read_grid incorrect')

    def test_create_gemfile(self):
        tmpfile = 'test.gem'
        s = gpf.gemread.create_gemfile(tmpfile, gemfile, 4312)
        self.assertEqual(s, 0, 'create_gemfile failure')
        self.assertEqual(os.path.getsize(tmpfile), 210944,
                         'create_gemfile: wrong size')
        os.remove(tmpfile)

    def test_write_gemfile(self):
        tmpfile = 'test.gem'
        s = gpf.gemread.create_gemfile(tmpfile, gemfile, 4312)
        grid = np.ones((self.nx,self.ny), np.float32, 'F')
        s = gpf.gemread.write_grid(tmpfile, '150610/1200F000', '', 850, -1,
                                   'PRES', 'TMPK', grid)
        self.assertEqual(s, 0, 'write_gemfile failure')
        self.assertEqual(os.path.getsize(tmpfile), 235520,
                         'write_gemfile: wrong size')
        os.remove(tmpfile)

suite = unittest.TestLoader().loadTestsFromTestCase(GempakTestCase)
unittest.TextTestRunner(verbosity=2).run(suite)
