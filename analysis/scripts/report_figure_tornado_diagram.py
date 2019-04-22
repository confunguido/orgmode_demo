#!/usr/local/bin/python3
# #======================================================================
# Author: Guido España
# Date: 2019/04/12
# #======================================================================
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
# from matplotlib.collections import PathCollection
# from matplotlib.patches import Rectangle

psa_cost_eff_data = pd.read_csv("../output/report_table_psa_results.csv",
                                sep='\s*,\s*', engine="python")

psa_cost_eff_data = psa_cost_eff_data.assign(
    ICER_min=lambda x: x['ICER_min'] / x['GDP'],
    ICER_max=lambda x: x['ICER_max'] / x['GDP'],
    ICER_default=lambda x: x['ICER_default'] / x['GDP'])

hh = 8
vv = 4.0
my_dpi = 1.0

fg = plt.figure(figsize=(hh/my_dpi, vv/my_dpi))
ax = fg.add_axes([0.1, 0.1, 0.8, 0.8])

y_vector = np.arange(psa_cost_eff_data.shape[0]) + 0.5
ax.plot(psa_cost_eff_data.loc[[0, 1], 'ICER_default'] , [0, max(y_vector)+0.5],
        linestyle='-', linewidth=1, c='k')

for p in range(psa_cost_eff_data.shape[0]):
    # ax.barh([p + 0.5, p + 0.5],
    #        list(psa_cost_eff_data.loc[p, ['ICER_min', 'ICER_max']]),
    #        align='center', color='#FF0000')
    xbar = list(psa_cost_eff_data.loc[p, ['ICER_min', 'ICER_max']])
    ax.plot(xbar, [p + 0.5, p + 0.5], linestyle="-",
            color="#1c9099AA", linewidth=40)
    max_align = "left"
    min_align = "right"
    text_offset = 0.3
    x1 = xbar[1] + text_offset
    x0 = xbar[0] - text_offset
    if xbar[1] < xbar[0]:
        min_align = "left"
        max_align = "right"
        x1 = xbar[1] - text_offset
        x0 = xbar[0] + text_offset
    ax.text(x0, p+0.5, '%.2f' % (psa_cost_eff_data.loc[p, 'min']),
            va="center", ha=min_align)
    ax.text(x1, p+0.5, '%.2f' % (psa_cost_eff_data.loc[p, 'max']),
            va="center", ha=max_align)
    # ax.plot(psa_cost_eff_data.loc[p, 'ICER_default'], p + 0.5, 'ko')
    print(p)

xmin, xmax = ax.get_xlim()
ymin, ymax = ax.get_ylim()
xmax *= 1.1
xmin -= (xmax - xmin)*0.1
ax.set_yticks(y_vector)
ax.set_yticklabels(psa_cost_eff_data['parameter'])
ax.set_xlim(xmin, xmax)
ax.set_ylim(0, max(y_vector) + 0.5)
ax.set_title('Sensitivity of cost-effectiveness analysis')
ax.set_xlabel('ICER/GDP(31,364 USD)')
labels3 = [str(x * -1) if x < 0 else str(x) for x in ax.get_xticks()]
ax.set_xticklabels(labels3)
plt.savefig("../figures/report_figure_tornado_diagram.jpeg", format='jpeg')
plt.close(fg)

# How to format the X labels?
