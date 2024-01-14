---
title:                "Python recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why
The ability to read and manipulate text files is a fundamental skill for any programmer. Whether you're looking to extract data or parse through large amounts of text, understanding how to read a text file will greatly enhance your coding capabilities.

## How To
Reading a text file in Python is a straightforward process. We will use the built-in `open()` function to open the file and then use the `read()` method to read its contents. Let's take a look at an example:

```Python
# Open the text file
file = open('sample.txt', 'r')

# Read the entire file
content = file.read()

# Print the contents
print(content)

# Close the file
file.close()
```

In the example above, we first use the `open()` function to open the `sample.txt` file in read-only mode using the `r` parameter. Then, we use the `read()` method to read the file's contents and store it in a variable called `content`. Finally, we print the contents and close the file using the `close()` method.

It's important to note that the `open()` function also has the ability to specify the encoding type, which can be useful when dealing with text files that may contain non-standard characters.

## Deep Dive
When reading a text file in Python, there are a few important things to keep in mind.

Firstly, the `read()` method returns a string, so if you want to work with the contents of the file as a list of lines, you can use the `readlines()` method instead. This will create a list where each element corresponds to a line in the text file.

Additionally, it's important to always close the file after you have finished reading it. This ensures that any resources used by the file are properly released and helps with managing memory usage.

Lastly, when dealing with large text files, it is more memory efficient to read the file line by line using a `for` loop rather than reading the entire file at once using the `read()` or `readlines()` methods.

```Python
# Open the text file
file = open('sample.txt', 'r')

# Read file line by line
for line in file:
    # Do something with each line
    print(line)

# Close the file
file.close()
```

## See Also
- [Official Python documentation on reading and writing files](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Real Python article on working with files in Python](https://realpython.com/read-write-files-python/)
- [GeeksforGeeks tutorial on reading and writing files in Python](https://www.geeksforgeeks.org/reading-writing-text-files-python/)

By following these simple steps, you now have the knowledge and tools to easily read and manipulate text files in your Python programs. Happy coding!