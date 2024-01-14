---
title:                "Python recipe: Searching and replacing text"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Have you ever found yourself in the tedious task of manually replacing text in a document or code? Not only is it time-consuming and monotonous, but it also leaves room for human error. Luckily, with the power of Python programming, you can easily automate this task and save yourself both time and effort.

## How To

To search and replace text in Python, we will use the `string.replace()` method. This method takes in two parameters: the old text and the new text. Let's look at an example:

```Python
# Define a string
text = "Hello world!"

# Replace "world" with "Python"
new_text = text.replace("world", "Python")

# Print the new string
print(new_text)
```
Output:
```
Hello Python!
```

We can also use the `string.replace()` method to replace multiple instances of the same text by adding a third parameter for the maximum number of replacements.

```Python
# Define a string with multiple instances of "Python"
text = "Python is the best language to learn Python"

# Replace "Python" with "Java" with a maximum of 1 replacement
new_text = text.replace("Python", "Java", 1)

# Print the new string
print(new_text)
```
Output:
```
Java is the best language to learn Python
```

We can also use this method to replace text in a text file. Let's say we have a text file named "example.txt" with the following content:

```
Hello world!
This is just a simple example.
```

We can use the `readlines()` and `writelines()` methods to read and replace text in the file.

```Python
# Open the file in read mode
file = open("example.txt", "r")

# Read the lines and store them in a list
lines = file.readlines()

# Replace "example" with "sample"
for index, line in enumerate(lines):
    lines[index] = line.replace("example", "sample")

# Open the file in write mode
file = open("example.txt", "w")

# Write the updated lines back to the file
file.writelines(lines)

# Close the file
file.close()
```

The contents of the file will now be:

```
Hello world!
This is just a simple sample.
```

## Deep Dive

There are various other methods in Python for searching and replacing text, such as using regular expressions with the `re` library. This provides more advanced and flexible ways of searching and replacing text. You can also specify case sensitivity, reordering of words, and much more using regular expressions.

Additionally, you can also use the `str.translate()` method to replace characters or words in a string by mapping them to new values.

## See Also

- [Python string methods](https://www.w3schools.com/python/python_ref_string.asp)
- [Python regular expressions](https://docs.python.org/3/library/re.html)
- [Python string translate method](https://www.geeksforgeeks.org/python-string-translate/)

Now you can say goodbye to manual text replacement and let Python handle it for you! Happy coding!