---
date: 2024-01-20 17:50:08.463610-07:00
description: String interpolation lets you insert values into a string. It's handy
  for creating custom messages, automating commands, and scripting like a boss.
lastmod: '2024-03-13T22:45:00.230677-06:00'
model: gpt-4-1106-preview
summary: String interpolation lets you insert values into a string. It's handy for
  creating custom messages, automating commands, and scripting like a boss.
title: Interpolating a string
---

{{< edit_this_page >}}

## What & Why?
String interpolation lets you insert values into a string. It's handy for creating custom messages, automating commands, and scripting like a boss.

## How to:
Bash strings play nice with variables. Drop a variable into a string with some curly braces, and you're golden.

```Bash
name="World"
greeting="Hello, ${name}!"
echo $greeting
```

Output:
```
Hello, World!
```

Bash says, "Keep it flexible." Change `name`, and your greeting follows suit.

```Bash
name="Bash Pros"
greeting="Hello, ${name}!"
echo $greeting
```

Output:
```
Hello, Bash Pros!
```

## Deep Dive
Back in the day, programmers glued strings together with concatenation. It got messy. String interpolation swooped in like a superhero for cleaner, more readable code.

Bash, unlike some other languages, doesn't fuss—just a dollar sign and some braces. Other languages dress it up with special syntax or functions. In Bash, it's all about those braces and the occasional escape character if you're feeling fancy.

Some alternatives? Sure, you can concatenate or use `echo` without braces if you're not doing anything complex. But why settle?

As for implementation, when Bash sees `${}`, it grabs the variable value and swaps it in, no questions asked. This makes sure what you see (in your code) is what you get (in your output).

## See Also
For more on string magic:

- Bash String Manipulation: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
- Stack Overflow (practical examples for real-world problems): https://stackoverflow.com/questions/tagged/bash
