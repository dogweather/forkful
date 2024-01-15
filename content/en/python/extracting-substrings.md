---
title:                "Extracting substrings"
html_title:           "Python recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why 
Substrings are an essential concept in computer programming, especially when working with strings. They allow us to extract specific parts of a string, which can be crucial for data manipulation and analysis. In this article, we will explore how to use Python to extract substrings and why it is an important skill for any programmer.

## How To
To extract substrings in Python, we use the slice notation. This notation allows us to specify the start and end index of the substring we want to extract. Let's look at an example:

```Python
# Create a string variable
my_string = "Hello World"

# Extract the substring "World"
my_substring = my_string[6:]

# Print the substring
print(my_substring)

# Output: World
```

In the code above, we have a string variable called `my_string` that contains the phrase "Hello World". Using the slice notation, we extract the substring starting at index 6 (which corresponds to the letter "W") until the end of the string. Then, we assign this substring to a new variable called `my_substring` and print it. 

We can also specify the start and end index using negative numbers. In this case, the counting starts from the end of the string. Let's see an example:

```Python
# Create a string variable
my_string = "Hello World"

# Extract the substring "Hello"
my_substring = my_string[:5]

# Print the substring
print(my_substring)

# Output: Hello
```

In the code above, we specify the start and end index as 0 and 5, respectively, to extract the substring "Hello" from the beginning of the string.

We can also use the slice notation to extract multiple substrings from a string. In this case, we separate each substring with a colon. Let's look at an example:

```Python
# Create a string variable
my_string = "1,2,3,4,5"

# Extract the substrings "2" and "4"
my_substrings = my_string[2:3], my_string[8:]

# Print the substrings
print(my_substrings)

# Output: (2, 4)
```

In the code above, we extract two substrings from the string `my_string` by specifying their respective start and end indexes.

## Deep Dive 
The slice notation not only allows us to extract substrings but also provides flexibility and control over the extracted string. We can specify the step size in addition to the start and end indexes. This allows us to skip over characters or extract every nth character. Let's see an example:

```Python
# Create a string variable
my_string = "abcdef"

# Extract every other character from the string
my_substring = my_string[::2]

# Print the substring
print(my_substring)

# Output: ace
```

In the code above, we use a step size of 2, which tells Python to extract every other character from the string, resulting in the substring "ace".

We can also use a negative step size to extract substrings in reverse order. Let's look at an example:

```Python
# Create a string variable
my_string = "abcdef"

# Extract the string in reverse order
my_substring = my_string[::-1]

# Print the substring
print(my_substring)

# Output: fedcba
```

In the code above, we use a negative step size to extract the string in reverse order, resulting in the substring "fedcba". 

## See Also
- [Python String Methods](https://www.w3schools.com/python/python_strings.asp) 
- [Python Slice Notation](https://www.geeksforgeeks.org/string-slicing-in-python/)

And that's all you need to know about extracting substrings in Python! With the slice notation, you can manipulate strings in a variety of ways for your programming needs. Practice using these techniques and take your string manipulation skills to the next level.