---
title:                "Python recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Have you ever come across a situation where you needed to delete characters from a string that matched a specific pattern? Maybe you were working with a large dataset and needed to clean it up, or maybe you were trying to format user input in a specific way. Whatever the reason may be, understanding how to delete characters matching a pattern in Python can be a useful skill to have in your programming arsenal.

## How To

To delete characters from a string that match a pattern, we can use the `re` module in Python. First, we need to import the module into our code:

```Python
import re
```

Next, we can use the `re.sub()` function to substitute the matching characters with an empty string. Let's say we have a string `sentence` that contains the word "Python" multiple times:

```Python
sentence = "I love Python programming. Python is a great language to learn."
```

If we want to delete all instances of the word "Python" from our string, we can use the following code:

```Python
cleaned_sentence = re.sub("Python", "", sentence)
print(cleaned_sentence)
```

The output would be:

`I love programming. is a great language to learn.`

As you can see, all instances of "Python" have been removed from our string. We can also use regular expressions to specify a more complex pattern to match. For example, if we only want to remove the word "Python" if it is followed by a colon, we can use the following code:

```Python 
cleaned_sentence = re.sub("Python:", "", sentence)
print(cleaned_sentence)
```

The output would be:

`I love programming. Python is a great language to learn.`

Notice that the second instance of "Python" was not removed because it was not followed by a colon. This demonstrates the power of using regular expressions to match specific patterns.

## Deep Dive

Now, let's take a deeper look at how the `re.sub()` function works. The first argument of this function is the pattern we want to match, and the second argument is what we want to replace the matching characters with. In the examples above, we used an empty string as the replacement, which essentially deletes the matching characters. However, we can use any string as the replacement, including other characters or even another word.

We can also specify an optional third argument to limit the number of replacements that are made. For example, if we only want to replace the first instance of "Python" in our string, we can do so by using `re.sub("Python", "", sentence, 1)`.

To learn more about the `re` module and regular expressions in Python, check out the official [documentation](https://docs.python.org/3/library/re.html).

## See Also

- [A Beginner's Guide to Regular Expressions in Python](https://realpython.com/regex-python/)
- [Introduction to the `re` Module in Python](https://www.geeksforgeeks.org/python-regex-cheat-sheet/)
- [Cleaning and Manipulating Text with Regular Expressions](https://towardsdatascience.com/cleaning-and-manipulating-text-with-regular-expressions-fd1394330c8e)

Happy coding!