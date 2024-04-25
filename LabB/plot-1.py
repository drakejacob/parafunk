#!/usr/bin/env python

import json
import sys
import numpy as np

import matplotlib

matplotlib.use('Agg') # For headless use

import matplotlib.pyplot as plt

benchmark = sys.argv[1]
data_sizes = list(map(int, sys.argv[2:]))

multi_filename = '{}-multi.json'.format(benchmark)
c_filename = '{}-c.json'.format(benchmark)

multi_json = json.load(open(multi_filename))
c_json = json.load(open(c_filename))

multi_measurements = multi_json['{}.fut'.format(benchmark)]['datasets']
c_measurements = c_json['{}.fut'.format(benchmark)]['datasets']

multi_runtimes = [ np.mean(multi_measurements['two_{}_i32s'.format(n)]['runtimes']) / 1000
                    for n in data_sizes ]
c_runtimes = [ np.mean(c_measurements['two_{}_i32s'.format(n)]['runtimes']) / 1000
               for n in data_sizes ]
speedups = list(map(lambda x, y: x / y, c_runtimes, multi_runtimes))

fig, ax1 = plt.subplots()
multi_runtime_plot = ax1.plot(data_sizes, multi_runtimes, 'b-', label='Multicore runtime')
c_runtime_plot = ax1.plot(data_sizes, c_runtimes, 'g-', label='Sequential runtime')
ax1.set_xlabel('Input size')
ax1.set_ylabel('Runtime (ms)', color='k')
ax1.tick_params('y', colors='k')
plt.xticks(data_sizes, rotation='vertical')
ax1.semilogx()
ax2 = ax1.twinx()
speedup_plot = ax2.plot(data_sizes, speedups, 'k-', label='Multicore speedup')
ax2.set_ylabel('Speedup', color='k')
ax2.tick_params('y', colors='k')

plots = multi_runtime_plot + c_runtime_plot + speedup_plot
labels = [p.get_label() for p in plots]
ax1.legend(plots, labels, loc=0)

fig.tight_layout()
plt.show()

plt.rc('text')
plt.savefig('{}.pdf'.format(benchmark), bbox_inches='tight')
