---
title:    "Go recipe: Searching and replacing text"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Why

There comes a time in every programmer's life when they need to do a massive search and replace in their code. Whether it's to fix a typo or to update a variable name throughout the codebase, this task can be time-consuming and tedious. Fear not, Go has some powerful tools to make this process much more efficient!

## How To

First, let's start with the basics. In Go, you can use the `strings.Replace()` function to replace all instances of a substring in a given string with another substring. This function takes in four arguments: the original string, the substring to be replaced, the new substring, and the number of replacements you want to make (use -1 to replace all). Let's take a look at an example:

```
Go strings.Replace("Hello World!", "World", "Universe", 1)

```

The output of this would be: "Hello Universe!". Notice how the `strings.Replace()` function only replaced the first instance of "World" with "Universe" because we specified a limit of 1.

But what if we want to replace all instances of a substring regardless of the number? In that case, we can simply use -1 as the limit:

```
Go strings.Replace("Hello World World!", "World", "Universe", -1)

```

The output would be: "Hello Universe Universe!".

But wait, there's more! Go also has a `strings.ReplaceAll()` function which does the same thing but automatically replaces all instances without needing the -1 limit. Let's try it out:

```
Go strings.ReplaceAll("Hello World World!", "World", "Universe")

```

The output would once again be: "Hello Universe Universe!".

Now, what if we want to do a case-insensitive search and replace? Go has got us covered with the `strings.ReplaceAll()` function. Let's see it in action:

```
Go strings.ReplaceAll(strings.ToLower("Hello World World!"), "world", "universe")

```

The output would also be: "Hello Universe Universe!".

## Deep Dive

Behind the scenes, the `strings.Replace()` and `strings.ReplaceAll()` functions use a package called `strings.Builder` to build the final string with the replacements made. This is a more efficient approach compared to using a regular `string` variable and constantly reassigning it with the replaced strings.

Furthermore, Go also has the `strings.NewReplacer()` function which allows us to replace multiple substrings with different substrings in one go. This is useful when you have a list of strings you want to replace, and it saves you from calling the `strings.Replace()` or `strings.ReplaceAll()` functions multiple times.

```
Go strings.NewReplacer("Hello", "Hi", "World", "Universe").Replace("Hello World!")

```

Output: "Hi Universe!".

## See Also

- [Go Strings Package Documentation](https://golang.org/pkg/strings/)
- [Go Strings.Builder Package Documentation](https://golang.org/pkg/strings/#Builder)
- [Go Strings.Replacer Package Documentation](https://golang.org/pkg/strings/#Replacer)