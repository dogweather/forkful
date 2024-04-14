---
date: 2024-02-03 19:02:33.737797-07:00
description: "How to: Bash does not have a built-in function specifically for capitalizing\
  \ strings, but you can accomplish this task using parameter expansion or\u2026"
lastmod: 2024-04-14
model: gpt-4-0125-preview
summary: Bash does not have a built-in function specifically for capitalizing strings,
  but you can accomplish this task using parameter expansion or external tools like
  `awk`.
title: Capitalizing a string
weight: 2
---

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
