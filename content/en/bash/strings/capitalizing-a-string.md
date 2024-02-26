---
date: 2024-02-03 19:02:33.737797-07:00
description: "Capitalizing a string in Bash involves transforming the first character\
  \ of the string to uppercase while leaving the rest of the string unchanged. This\u2026"
lastmod: '2024-02-25T18:49:56.667165-07:00'
model: gpt-4-0125-preview
summary: "Capitalizing a string in Bash involves transforming the first character\
  \ of the string to uppercase while leaving the rest of the string unchanged. This\u2026"
title: Capitalizing a string
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string in Bash involves transforming the first character of the string to uppercase while leaving the rest of the string unchanged. This technique is commonly used for formatting output or complying with coding conventions that require certain strings to start with a capital letter for readability or stylistic preferences.

## How to:

Bash does not have a built-in function specifically for capitalizing strings, but you can accomplish this task using parameter expansion or external tools like `awk`. Here are a few ways to capitalize a string in Bash:

**Using Parameter Expansion:**

This method manipulates the string directly in the shell.

```bash
str="hello world"
capitalized="${str^}"
echo "$capitalized"
```
Output:
```
Hello world
```

**Using `awk`:**

`awk` is a powerful text processing tool available on most Unix-like operating systems, which can be utilized to capitalize strings.

```bash
str="hello world"
echo "$str" | awk '{print toupper(substr($0, 1, 1)) tolower(substr($0, 2))}'
```
Output:
```
Hello world
```

**Using `sed`:**

For a more traditional approach, `sed` can be employed to capitalize the first letter of a string. However, it's a bit more complex compared to the previous methods.

```bash
str="hello world"
echo "$str" | sed 's/./\u&/'
```
Output:
```
Hello world
```

These snippets demonstrate how to capitalize the first letter of a string in Bash, highlighting the flexibility of shell scripting when manipulating text.
