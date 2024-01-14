---
title:    "Python recipe: Searching and replacing text"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Searching and replacing text is a common task in any programming language, and Python is no exception. This process allows for quick and efficient changes to be made in a large body of text or code, saving time and effort in the long run.

## How To

To search and replace text in Python, the `replace()` method can be used. This method takes two arguments, the text to be replaced and the text to replace it with.

For example, if we have a string `sentence = "I like to eat apples"`, and we want to change "apples" to "oranges", we can use the `replace()` method as follows:

```Python
new_sentence = sentence.replace("apples", "oranges")
print(new_sentence)
```

The output will be:

`I like to eat oranges`

The `replace()` method can also be used on input from the user. For instance, if we want to replace a certain word with another word based on user input, we can use the `input()` function and store the user's input in a variable. This can then be passed as an argument to the `replace()` method.

```Python
search_word = input("Enter the word to be replaced: ")
replace_word = input("Enter the word to replace it with: ")
new_sentence = sentence.replace(search_word, replace_word)
print(new_sentence)
```

## Deep Dive

The `replace()` method is case-sensitive, meaning if the text to be replaced is in uppercase, it will only replace those specific characters. To replace all instances of a text, regardless of case, we can use the `lower()` method in conjunction with `replace()`.

```Python
sentence = "I like to eat apples but i also like APPLE juice"
new_sentence = sentence.lower().replace("apple", "orange")
print(new_sentence)
```

The output will be:

`i like to eat oranges but i also like orange juice`

Additionally, the `replace()` method can also be used on specific parts of a string. To do this, we can use string slicing to specify the starting and ending index of the portion we want to replace.

```Python
sentence = "I like to eat apples"
new_sentence = sentence[:7].replace("like", "love") + sentence[7:]
print(new_sentence)
```

The output will be:

`I love to eat apples`

## See Also

- [Python String Methods](https://www.w3schools.com/python/python_ref_string.asp)
- [Official Python Documentation on `replace()`](https://docs.python.org/3/library/stdtypes.html#str.replace)