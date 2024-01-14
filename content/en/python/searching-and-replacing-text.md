---
title:    "Python recipe: Searching and replacing text"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Why 
Searching and replacing text is a common task in any programming language. It allows you to quickly and efficiently make changes to your code without having to manually go through each line. In Python, there are various ways to search and replace text, making it a valuable skill for any programmer.

## How To 
To perform a search and replace in Python, you can use the `replace()` function. This function takes in two parameters, the old string and the new string, and returns a new string with the specified replacement. For example:

```Python
sentence = "I love programming in Python!"
new_sentence = sentence.replace("Python", "JavaScript")

print(new_sentence)
```

This will output: `I love programming in JavaScript!`

You can also use `replace()` to replace multiple instances of a string. For example:

```Python
sentence = "Python is my favorite programming language. Python is so versatile."
new_sentence = sentence.replace("Python", "JavaScript", 2)

print(new_sentence)
```

This will output: `JavaScript is my favorite programming language. JavaScript is so versatile.` Notice how the third parameter specifies the number of replacements to make. In this case, we only want to replace the first two instances of "Python".

Another useful function for search and replace is `re.sub()` from the `re` module. This function allows you to use regular expressions to search and replace text. For example:

```Python
import re

sentence = "I love using Python for web development!"
new_sentence = re.sub(r'Python', 'JavaScript', sentence)

print(new_sentence)
```

This will output: `I love using JavaScript for web development!` Regular expressions can give you more flexibility when searching for patterns in your text.

## Deep Dive 
When using `replace()` or `re.sub()`, it's important to note that the original string is not modified. Instead, a new string with the replacement is returned. If you want to modify the original string, you can assign the new string to the same variable, like this:

```Python
sentence = "I love coding in Python!"
sentence = sentence.replace("Python", "JavaScript")

print(sentence)
```

This will output: `I love coding in JavaScript!`

It's also worth mentioning that `replace()` and `re.sub()` are case-sensitive. So if you want to replace a string, make sure you use the exact same casing.

## See Also 
Here are some additional resources for learning about searching and replacing text in Python:

- [Python String Methods](https://www.programiz.com/python-programming/methods/string)
- [Regular Expressions in Python](https://www.geeksforgeeks.org/python-regex-tutorial/)
- [Regular Expression Syntax Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/python)