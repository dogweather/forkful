---
title:    "Fish Shell recipe: Using regular expressions"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

##Why

Regular expressions are a powerful tool in programming that allow you to search, manipulate, and validate text data. They provide a concise and efficient way to match patterns within a string, making tasks such as data cleaning and text parsing much easier. By learning how to use regular expressions, you can greatly improve your coding productivity and enhance the functionality of your scripts.

##How To

Coding with regular expressions in Fish Shell is made easy with the built-in `string` commands. Let’s take a look at a few examples of how to use these commands to search for patterns within a string.

First, we’ll create a variable `my_string` with a string of text that we want to search through.

```
set my_string "I love programming with regular expressions!"
```

Now, let’s use the `string match` command to search for the word “regular” within our string.

```
string match -rq "regular" $my_string
```

The `-r` flag enables regular expression matching and the `-q` flag tells Fish Shell to only output a boolean value. So, our command will return `1` if the word “regular” is found and `0` if it is not.

We can also use regular expressions for more complex matches. Let’s say we want to extract all the numbers from a string.

```
set my_string "I have 10 fingers and 2 eyes"
string replace -q ".* (\d+).*" '$1' $my_string
```

The `string replace` command allows us to replace matched patterns with a specified value. In this case, we are using the regular expression `.* (\d+).*` to match any string followed by a space and one or more digits, and then replacing the entire string with just the number. The output of this command will be `10 2`.

##Deep Dive

Regular expressions consist of special characters and symbols that represent a specific pattern to match within a string. Some common special characters and their meanings include:

- `.` : Matches any single character
- `*` : Matches the preceding pattern zero or more times
- `+` : Matches the preceding pattern one or more times
- `?` : Makes the preceding pattern optional
- `[]` : Matches any single character within the brackets
- `()` : Creates a matching group for use in replacement

To learn more about the different characters and symbols used in regular expressions, check out the Fish Shell documentation or various online resources.

##See Also

- [Fish Shell Official Documentation](https://fishshell.com/docs/current/commands.html#string)
- [Regular Expressions Cheat Sheet](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)
- [Online Regular Expression Tester](https://regex101.com/)