import numpy as np
import gempakf as gp
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap

class Dataset:
    def __init__(self, gemfile):
        self.gemfile = gemfile

        n = gp.gemread.get_num_grids(gemfile)

        # Local variables
        gdattm = np.zeros((20,n,2), np.int8, 'F')
        level = np.zeros((n,2), np.int32, 'F')
        ivcord = np.zeros(n, np.int32)
        vcord = np.zeros((4,n), np.int8, 'F')
        parm = np.zeros((12,n), np.int8, 'F')

        self.max_grids, self.num_grids, self.nx, self.ny, self.proj, self.ang, \
            self.lllat, self.lllon, self.urlat, self.urlon = \
            gp.gemread.ggi(gemfile, gdattm, level, ivcord, vcord, parm)

        self.proj = self.proj.strip()

        self.datainfo = []
        for i in range(self.num_grids):
            dattim = [gdattm[:,i,0].view('a20')[0].strip(), \
                          gdattm[:,i,1].view('a20')[0].strip()]
            lev = [level[i,0], level[i,1]]
            vc = vcord[:,i].view('a4')[0].strip()
            fun = parm[:,i].view('a12')[0].strip()
            datarow = {'gdattim': dattim, 'glevel': lev, 'gvcord': vc, 'gfunc': fun}
            self.datainfo.append(datarow)

    def grid_from_num(self, num):
        grid = np.zeros((self.nx,self.ny), np.float32, 'F')
        gp.gemread.read_grid(self.gemfile, \
                                 self.datainfo[num]['gdattim'][0], \
                                 self.datainfo[num]['gdattim'][1], \
                                 self.datainfo[num]['glevel'][0], \
                                 self.datainfo[num]['glevel'][1], \
                                 self.datainfo[num]['gvcord'], \
                                 self.datainfo[num]['gfunc'], grid)
        return grid.transpose()

    def grid_from_dict(self, d):
        grid = np.zeros((self.nx,self.ny), np.float32, 'F')
        gp.gemread.read_grid(self. gemfile, d['gdattim'][0], d['gdattim'][1], \
                                 d['glevel'][0], d['glevel'][1], d['gvcord'], \
                                 d['gfunc'], grid)
        return grid.transpose()

def map_for_dataset(dset, res='l'):
    if dset.proj=='LCC':
        m = Basemap(llcrnrlon=dset.lllon, llcrnrlat=dset.lllat, urcrnrlon=dset.urlon, \
                        urcrnrlat = dset.urlat, projection='lcc', lat_1=dset.ang[0], \
                        lat_2=dset.ang[2], lon_0=dset.ang[1], resolution=res)
    else:
        print 'Sorry, this projection is not yet supported.  :-('
        m = 0

    return m
    
if __name__ == "__main__":
    gemdata = Dataset('/ldmdata/gempak/model/nam/14032812_nam212.gem')
    print gemdata.datainfo[0]
    arr = gemdata.grid_from_dict(gemdata.datainfo[10])
    m = map_for_dataset(gemdata)
    m.drawcountries()
    m.drawcoastlines()
    m.drawstates()
    x = np.linspace(m.xmin,m.xmax,gemdata.nx)
    y = np.linspace(m.ymin,m.ymax,gemdata.ny)
    xmesh, ymesh = np.meshgrid(x, y)
    m.contourf(xmesh,ymesh,arr)
    plt.show()
