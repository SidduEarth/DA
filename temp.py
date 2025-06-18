import pandas as pd

# Define the path to your CSV file
# If your file is in the same directory as your Python script, you can just use 'weather_data.csv'
# If it's in a different location, replace 'weather_data.csv' with the full path,
# for example: r'D:\R programming\weather_data.csv' (the 'r' before the string handles backslashes)
file_path = 'weather_data.csv' # Or r'D:\R programming\weather_data.csv'

try:
    # Load the CSV file into a pandas DataFrame
    df = pd.read_csv(file_path)

    # Print the head (first 5 rows by default) of the DataFrame
    print(df.head())

except FileNotFoundError:
    print(f"Error: The file '{file_path}' was not found.")
    print("Please ensure the file is in the correct directory or provide the full and correct path.")
except Exception as e:
    print(f"An error occurred: {e}")