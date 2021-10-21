from datetime import datetime
from random import randrange

from matplotlib import pyplot
from matplotlib.animation import FuncAnimation

init_notebook_mode(connected=True)

# Read data
x_data, y_data = [], []

figure = pyplot.figure()
line, = pyplot.plot_date(x_data, y_data, '-')

def update(frame):
    x_data.append(datetime.now())
    y_data.append(randrange(0, 100))
    line.set_data(x_data, y_data)
    figure.gca().relim()
    figure.gca().autoscale_view()
    return line,

animation = FuncAnimation(figure, update, interval=200)

pyplot.show()

import matplotlib.pyplot as plt

fig, ax = plt.subplots()
y1 = []
for i in range(50):
    y1.append(i)  # Cada iteración, dibuja i en y1 y dibuja
    ax.cla()   # Clave clara
    ax.bar(y1, label='test', height=y1, width=0.3)
    ax.legend()
    plt.pause(0.1)
