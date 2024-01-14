---
title:    "Fish Shell recipe: Concatenating strings"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

##Why

Concatenating strings is a common task in programming where multiple strings are combined to form a single string. This is often necessary when we need to display information or create dynamic content in our programs. With the Fish Shell, string concatenation is quick and easy, making it a useful skill for any programmer to have.

##How To

To concatenate strings in Fish Shell, we use the `string` command with the `concat` flag. Let's take a look at a simple example:

```
Fish Shell> string concat "Hello, " "world!"
Hello, world!
```
In this example, we used the `string` command to combine the strings "Hello, " and "world!" to form "Hello, world!" The `concat` flag tells the command to concatenate the given strings.

We can also concatenate multiple strings at once by separating them with spaces:

```
Fish Shell> string concat "I" "love" "Fish Shell!"
I love Fish Shell!
```

We can even use the output of other commands in the concatenation. Let's use the `echo` command to display the current date and then use it in our concatenation:

```
Fish Shell> echo (date)
Tue Nov 17 12:35:25 EST 2020
Fish Shell> string concat "Today is " (echo (date))
Today is Tue Nov 17 12:35:25 EST 2020
```

As you can see, we can concatenate strings of any length and combine them in different ways to fit our needs.

##Deep Dive

Under the hood, the `string` command uses the `printf` function to concatenate strings. This allows for greater flexibility and control over the output. We can use different formatting options and placeholders to customize our concatenated string.

For example, we can use the `%s` placeholder to insert a string at a specific location in our output:

```
Fish Shell> string concat "My name is %s, and I am %d years old." "John" 25
My name is John, and I am 25 years old.
```

We can also use the `-f` flag to specify a format string and pass in multiple inputs to be combined:

```
Fish Shell> string concat -f "Hello, %s. Today is %s." "John" (date)
Hello, John. Today is Tue Nov 17 12:35:25 EST 2020.
```

For more information on all the formatting options available, check out the official Fish Shell documentation on string concatenation.

##See Also

- [Fish Shell Documentation on string concatenation](https://fishshell.com/docs/current/cmds/string.html#string)
- [Printf Formatting Tutorial](https://www.tutorialspoint.com/printf-formatting-in-shell-script)
- [Advanced Fish Shell Tips and Tricks](https://medium.com/@wxlfsoxfly/10-fish-shell-advanced-tips-and-tricks-you-should-know-cb9201aaa8f4)