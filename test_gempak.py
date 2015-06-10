import unittest
import gempak as gp

gemfile = 'nam211.gem'

class GempakTestCase(unittest.TestCase):
    def setUp(self):
        self.data = gp.Dataset(gemfile)

    def tearDown(self):
        self.data = None

    def test_ang(self):
        self.assertEqual(self.data.ang[0], 25, 'test_ang: ang[0] incorrect')
        self.assertEqual(self.data.ang[1], -95, 'test_ang: ang[1] incorrect')
        self.assertEqual(self.data.ang[2], 25, 'test_ang: ang[2] incorrect')


suite = unittest.TestLoader().loadTestsFromTestCase(GempakTestCase)
unittest.TextTestRunner(verbosity=2).run(suite)
