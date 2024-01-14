---
title:    "Python recipe: Reading a text file"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Why

Python is a popular programming language used for a variety of applications, from web development to data analysis. One of its key features is its ability to easily manipulate text files, making it a useful tool for data processing tasks. In this blog post, we will explore the basics of reading a text file in Python, as well as some more advanced techniques.

## How To

Reading a text file in Python is a simple and straightforward process. Let's take a look at the basic code structure:

```Python
# Open the file
file = open("example.txt", "r")

# Read the file contents using the read() method
file_contents = file.read()

# Close the file
file.close()

# Print the contents of the file
print(file_contents)
```

In the above code, we first use the `open()` function to open the file in "read" mode. This tells Python that we want to read the contents of the file. Next, we use the `read()` method to read the file contents and store them in a variable called `file_contents`. Finally, we close the file using the `close()` method and print out the contents of the file using the `print()` function.

Now, let's take a look at a more advanced example where we read the file line by line and do some basic data processing:

```Python
# Open the file
file = open("example.txt", "r")

# Read the file line by line
for line in file:
    # Remove any trailing whitespace using the strip() method
    line = line.strip()

    # Split the line into a list using the split() method
    line_list = line.split()

    # Loop through the list and print out each word
    for word in line_list:
        print(word)

# Close the file
file.close()
```

In the above code, we use a `for` loop to iterate through each line of the file. Then, we use the `strip()` method to remove any trailing whitespace from each line, and the `split()` method to split the line into a list of words. Finally, we loop through the list and print out each word. This is just one example of how you can manipulate text files in Python - the possibilities are endless!

## Deep Dive

Now that we have covered the basics of reading a text file in Python, let's take a deeper dive into some additional concepts that may be useful.

### With Statement

In the previous examples, we used the `close()` method to close the file after we were done using it. However, this can be cumbersome and prone to errors. A better approach is to use the `with` statement, which automatically closes the file for us after we are done with it. Here's an example:

```Python
# Open the file using the with statement
with open("example.txt", "r") as file:
    # Read the file contents using the read() method
    file_contents = file.read()

# The file is automatically closed after this block of code
```

### Error Handling

When working with files, it is important to be prepared for errors that may occur. For example, if the file we are trying to open does not exist, our code will throw an error. To handle these situations, we can use try-except blocks. Here's an example:

```Python
try:
    # Open the file
    file = open("example.txt", "r")

    # Read the file contents using the read() method
    file_contents = file.read()

    # Print the contents of the file
    print(file_contents)

# Handle the FileNotFound error
except FileNotFoundError:
    print("File not found!")

# Finally, close the file
finally:
    file.close()
```

## See Also

- Official Python documentation on reading and writing files: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- RealPython tutorial on working with files in Python: https://realpython.com/read-write-files-python/#reading-and-writing-files-in-python