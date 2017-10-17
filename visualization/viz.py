"""Visualize the output of TrueSkill Through Time."""

import collections
import itertools
import matplotlib.pyplot as plt
import numpy as np


def read_csv(path):
    """Read data from a CSV file.

    Parameters
    ----------
    path : str
        Path to the CSV file produced by the `ChessAnalysis` program.

    Returns
    -------
    data : dict
        Dictionary mapping player names to a sequence of rating values.
    """
    data = collections.defaultdict(list)
    with open(path) as f:
        next(f)  # First line is header.
        for line in f:
            name, year, mu, sigma = line.strip().split(",")
            data[name].append((int(year), float(mu), float(sigma)))
    return data


def plot_skills(data, player_names):
    """Read data from a CSV file.

    Parameters
    ----------
    data : dict
        Dictionary as returned by `read_csv`.
    player_names : list
        List of players to be included in the plot.

    Returns
    -------
    (fig, ax) : tuple
        The `matplotlib` figure and axes handles.
    """
    colors = itertools.cycle(plt.cm.tab10(np.linspace(0, 1, 10)))
    fig, ax = plt.subplots(figsize=(16, 5))
    for name, color in zip(player_names, colors):
        ts, ms, ss = map(np.array, zip(*sorted(data[name])))
        ax.plot(ts, ms, color=color, label=name)
        ax.fill_between(ts, ms-ss, ms+ss, color=color, alpha=0.1)
    ax.legend(loc="lower right")
    ax.set_title("Evolution of rating over time (mean +/- 1 std. dev.)")
    ax.set_xlabel("year")
    ax.set_ylabel("Elo points")
    return fig, ax
