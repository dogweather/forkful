---
title:                "Searching and replacing text"
html_title:           "Python recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Searching and replacing text is a common task in many programming projects. It allows you to efficiently make changes in your code or data without having to manually go through each instance. In Python, the built-in `replace()` method makes it easy to perform this task.

## How To
To use the `replace()` method, you first need to have a string that you want to make changes to. Let's say we have the following string:
```Python
sentence = "I love pizza, but I also enjoy sushi."
```
Now, if we want to replace "pizza" with "tacos" in this sentence, we can use the `replace()` method:
```Python
new_sentence = sentence.replace("pizza", "tacos")
print(new_sentence)
```
The output will be:
```
I love tacos, but I also enjoy sushi.
```
As you can see, the `replace()` method replaced all occurrences of "pizza" in the original string with "tacos". It is also case-sensitive, so it will not replace "Pizza" or "PIZZA".

You can also specify the number of replacements you want to make by using the optional `count` parameter. For example, if we only want to replace the first instance of "pizza" in our sentence, we can do so by setting `count=1`:
```Python
new_sentence = sentence.replace("pizza", "tacos", 1)
print(new_sentence)
```
The output will be:
```
I love tacos, but I also enjoy sushi.
```

## Deep Dive
The `replace()` method is part of the string class in Python, which means it can only be used on strings. It takes two required parameters: `old` and `new`, which represent the text you want to replace and the text you want to replace it with.

Another important thing to note is that the `replace()` method does not change the original string, but instead returns a new string with the changes. This is why we had to assign the result to a variable in our examples.

It is also worth mentioning that the `replace()` method is not limited to just single characters or words. You can also use it to replace longer strings:
```Python
sentence = "I love coding, but I also enjoy hiking."
new_sentence = sentence.replace("coding", "programming")
print(new_sentence)
```
The output will be:
```
I love programming, but I also enjoy hiking.
```

## See Also
- [Python String Methods](https://www.w3schools.com/python/python_ref_string.asp)
- [Python String replace() Method](https://www.w3schools.com/python/ref_string_replace.asp)