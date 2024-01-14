---
title:    "Fish Shell recipe: Converting a string to lower case"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Why

One common task when working with strings in any programming language is converting them to a specific case, such as lower case. This can be helpful for things like user input validation or ensuring consistency in data processing. In this blog post, we will explore how to convert a string to lower case using the powerful Fish Shell.

## How To

Coding a string to lower case in Fish Shell is quite simple. Here's an example:

```
set my_string "HELLO WORLD"
echo $my_string | tr '[A-Z]' '[a-z]'
```

The first line sets the variable `my_string` to the string "HELLO WORLD". The second line uses the `tr` command to translate all the characters in `my_string` from uppercase to lowercase. 

The output of this code block would be `hello world`, as expected. 

Alternatively, instead of using the `tr` command, we can also use the `string tolower` command like this:

```
set my_string "HELLO WORLD"
string tolower $my_string
```

Both of these methods will achieve the same result of converting the string to lowercase.

## Deep Dive

Now, let's take a closer look at how the `tr` command works in this case. The `tr` command is used to translate, or replace, characters in a given string. In this example, we are replacing all uppercase letters to lowercase letters using the specified character ranges. `[A-Z]` indicates all uppercase letters, while `[a-z]` indicates all lowercase letters. 

It's worth noting that both `string tolower` and `tr` are case-sensitive. So if your string already contains lowercase letters, they will remain unchanged. Also, if your string contains characters outside of the specified range, they will also remain unchanged. 

Another thing to keep in mind is that these methods only work on one string at a time. If you need to convert multiple strings to lowercase, you will need to use a loop or a function.

## See Also

For more information on Fish Shell and string manipulation, check out these helpful resources:

- [Fish Shell official documentation](https://fishshell.com/docs/current/cmds/string.html) 
- [Fish Shell tricks: Convert text to lowercase or uppercase](https://medium.com/@prchandra/fish-shell-tricks-convert-text-to-lowercase-or-uppercase-d50c2734cfab) 
- [101 Bash Commands and Tips for Beginners to Experts](https://stackabuse.com/101-bash-commands-and-tips-for-beginners-to-experts/)