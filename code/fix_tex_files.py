import os
import re

# Directory containing the .tex files
directory = "./R_Output/"

# Pattern to search for
subs = {'\\{': '{', '\\}': '}', '$\\backslash$': '\\'}

# Function to replace the pattern in a file
def replace_pattern_in_file(file_path):
    with open(file_path, 'r') as file:
        content = file.read()
        for pattern, replacement in subs.items():
            content = content.replace(pattern, replacement)
    
    with open(file_path, 'w') as file:
        file.write(content)

# Iterate through .tex files in the directory
for root, _, files in os.walk(directory):
    for file_name in files:
        if file_name.endswith(".tex"):
            file_path = os.path.join(root, file_name)
            replace_pattern_in_file(file_path)
