---
title:    "Python recipe: Finding the length of a string"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why
In Python, finding the length of a string is a fundamental task that can be used in a variety of applications. Whether you're working with text-based data or manipulating user input, being able to determine the length of a string is a useful skill to have. In this blog post, we'll explore how to find the length of a string in Python and understand why it's an important concept to grasp.

## How To
To find the length of a string in Python, we can use the built-in function `len()`. This function takes a string as a parameter and returns the number of characters in that string. Let's take a look at an example:

```Python
sentence = "This is a sample string."
print(len(sentence))
```

The output of this code will be `23`, as there are 23 characters in the string "This is a sample string." 
We can also use `len()` to find the length of a string stored in a variable:

```Python
name = "Alice"
print(len(name))
```

The output of this code will be `5`, as there are 5 letters in the name "Alice."

## Deep Dive
Behind the scenes, the `len()` function is using an algorithm to calculate the length of a string. The function iterates through each character in the string and counts them until it reaches the end. This means that the function will return the same result for strings of the same length, regardless of the actual characters that make up the string.

It's also important to note that `len()` counts all characters, including spaces and punctuation marks. This can be useful in certain situations, but if you want to exclude certain characters from the count, you will need to implement additional logic in your code. 

## See Also
- [Official documentation for len() function](https://docs.python.org/3/library/functions.html#len)
- [GeeksforGeeks article on finding the length of a string](https://www.geeksforgeeks.org/python-string-length-len/) 
- [Real Python tutorial on string manipulation in Python](https://realpython.com/python-strings/)

By mastering the concept of finding the length of a string in Python, you will have a better understanding of how to manipulate and work with text data in your programs. So the next time you need to find the length of a string, you'll know exactly what to do. Happy coding!