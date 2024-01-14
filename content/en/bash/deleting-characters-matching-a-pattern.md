---
title:    "Bash recipe: Deleting characters matching a pattern"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why 

Deleting characters matching a certain pattern in a Bash program can be a useful tool when editing or manipulating text. It allows for quick and efficient removal of specific characters, making tasks like data cleaning or formatting much easier.

## How To 

To delete characters matching a pattern, we will use the Bash command `sed`. This command is used for editing or manipulating text files. 

We will first need to specify the pattern we want to match. This can be done by using regular expressions, which are a set of characters used to search and match specific patterns in strings. Regular expressions can be quite complex, but for this example, we will use a simple pattern of the letter "a".

To delete all characters matching the letter "a" from a text file, we will use the following command:

```
sed 's/a//g' file.txt
```

The `s` in this command stands for substitution, the first set of quotations after it specifies the pattern we want to match, and the second set of quotations is left empty to indicate that we want to replace the matched pattern with nothing. The `g` at the end of the command is used to indicate that we want to perform this substitution globally on the entire file.

Let's say our text file contains the following line: "I love apples and bananas." After running the `sed` command, the line would be changed to "I love pples nd bnns."

## Deep Dive 

The `sed` command has many different options and modifiers, making it a powerful tool for text manipulation. 

One useful modifier is the `-i` option, which allows us to directly edit the text file rather than just printing the changes to the terminal. This is helpful when we want to permanently delete the characters matching our pattern.

We can also use regular expressions to match more complex patterns, such as entire words or phrases. For example, to delete all instances of the word "apple" in our text file, we can use the following command:

```
sed 's/apple//g' file.txt
```

Additionally, we can use flags such as `&` to include the matched pattern in the substitution. So if we wanted to replace the word "apple" with "juicy apple", we could use the following command:

```
sed 's/apple/juicy &/g' file.txt
```

This would result in the line "I love juicy apples and bananas." 

## See Also 

- [Regular Expressions 101](https://regex101.com/) 
- [Bash Sed Command Examples](https://likegeeks.com/linux-sed-command/) 
- [The Power of Regular Expressions in Bash](https://unix.stackexchange.com/questions/530783/power-of-regular-expression-in-bash)