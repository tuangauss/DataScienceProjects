
JUPYTER
FAQ
import random
import numpy as np
import matplotlib.pyplot as plt
from tqdm import tqdm

def park_cars(road_length):
    """
    Simulate the Renyi's parking process on a road of given length.
    
    Args:
        road_length (float): The length of the road.
        
    Returns:
        int: The number of cars parked.
    """
    # List to store the segments of the road that are still available for parking
    available_segments = [(0, road_length)]
    
    # Count of parked cars
    parked_cars = 0
    
    # Continue until no segment is large enough to park a car
    while available_segments:
        # Filter out segments that are too small for a car
        available_segments = [(start, end) for start, end in available_segments if end - start >= 1]
        
        if not available_segments:
            break
        
        # Select a random segment based on its length
        segments_length = [end - start for start, end in available_segments]
        total_length = sum(segments_length)
        probabilities = [length / total_length for length in segments_length]
        
        segment_idx = np.random.choice(len(available_segments), p=probabilities)
        start, end = available_segments[segment_idx]
        
        # Select a random position within the segment to park the car
        pos = random.uniform(start, end - 1)
        
        # Park the car
        parked_cars += 1
        
        # Update available segments
        available_segments.pop(segment_idx)
        
        # Add segments to the left and right of the parked car
        if pos > start:
            available_segments.append((start, pos))
        if pos + 1 < end:
            available_segments.append((pos + 1, end))
    
    return parked_cars

def simulate_renyi_parking(road_lengths, iterations=10000):
    """
    Simulate the Renyi's parking process for different road lengths.
    
    Args:
        road_lengths (list): List of road lengths to simulate.
        iterations (int): Number of iterations for each road length.
        
    Returns:
        dict: Dictionary mapping road lengths to average ratios.
    """
    results = {}
    
    for length in road_lengths:
        print("Simulate Parking for road length:", length)
        total_cars = 0
        for _ in tqdm(range(iterations), desc=f"Road length: {length}"):
            cars_parked = park_cars(length)
            total_cars += cars_parked
        
        average_ratio = total_cars / (iterations * length)
        results[length] = average_ratio
    
    return results


# Road lengths to simulate
road_lengths = [10, 30, 50, 100, 500, 750, 1000]

# Run the simulation
print("Simulating Renyi's parking problem...")
print("This will compute the average ratio of cars parked to road length.")
print("The theoretical value of Renyi's parking constant is approximately 0.7476...")
results = simulate_renyi_parking(road_lengths, iterations = 1000)

# Publication-ready plotting with explicit LaTeX disabling
import matplotlib.pyplot as plt
import numpy as np

# Reset and explicitly disable LaTeX
plt.rcdefaults()
plt.rcParams['text.usetex'] = False
plt.rcParams['mathtext.fontset'] = 'dejavusans'  # Use a standard font
plt.rcParams['font.family'] = 'sans-serif'
plt.rcParams['font.sans-serif'] = ['DejaVu Sans', 'Arial', 'Helvetica']
plt.rcParams['figure.dpi'] = 300

# Create figure and axis with a simpler approach
fig, ax = plt.subplots(figsize=(8, 6))

# Plot the data
ax.plot(list(results.keys()), list(results.values()), 
        marker='o', markersize=8, 
        linestyle='-', linewidth=2,
        label='Simulation Results')

# Add the theoretical value line
ax.axhline(y=0.7476, color='red', linestyle='--', linewidth=2, 
           label="Renyi's Parking Constant (approx. 0.7476)")

# Mark each data point with its value
for x, y in zip(results.keys(), results.values()):
    ax.annotate(f'{y:.3f}', xy=(x, y), xytext=(0, 10), fontsize=8,
                textcoords='offset points', ha='center', va='bottom')

# Set axis scales and limits
ax.set_xscale('log')
ax.set_xlim(min(results.keys())*0.8, max(results.keys())*1.2)
ax.set_ylim(0.72, 0.755)  # Using fixed values to avoid any calculation issues

# Add labels and title
ax.set_xlabel('Road Length (x)', fontweight='bold')
ax.set_ylabel('Average Ratio (Cars Parked / Road Length)', fontweight='bold')
ax.set_title("Convergence to our magic Parking Constant", fontweight='bold')

# Add x-ticks at each tested road length
ax.set_xticks(list(results.keys()))
ax.set_xticklabels([f'{int(x)}' for x in results.keys()])

# Customize the grid
ax.grid(True, linestyle='--', alpha=0.7)

# Add a legend
ax.legend(loc='lower right')

# Add a text box with simulation details
textstr = 'Simulation Parameters:\n10,000 iterations per length\nCar length: 1 unit\nRandom uniform parking'
ax.text(0.05, 0.05, textstr, transform=ax.transAxes, fontsize=10,
        verticalalignment='bottom', bbox={'facecolor': 'white', 'alpha': 0.8, 'pad': 5})

# Adjust layout manually instead of using tight_layout()
plt.subplots_adjust(left=0.12, right=0.95, top=0.92, bottom=0.12)

# Save the figure in high quality
#plt.savefig('renyi_parking_constant.png', dpi=300)

# Show the plot
plt.show()
