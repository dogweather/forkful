---
title:                "Concatenating strings"
date:                  2024-01-20T17:34:16.150796-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenating strings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings in Bash means sticking two or more pieces of text together. Programmers do it to build up commands, create file paths, or simply format output text.

## How to:

Here's the fast track to giving your strings a snug hug in Bash:

```Bash
# Concatenating by placing strings next to each other
greeting="Hello, "
name="world!"
welcome=$greeting$name
echo $welcome  # Outputs: Hello, world!

# Using curly braces for clarity
version="version"
number=1
full_version=${version}_${number}
echo $full_version  # Outputs: version_1

# Concatenating with variables and literals
timestamp=$(date +%Y%m%d)  # Gets the current date in YYYYMMDD format
filename="backup_${timestamp}.tar.gz"
echo $filename  # Outputs: backup_20230315.tar.gz
```

## Deep Dive

Back in the days before GUIs ruled the land, command lines and scripts were the kings of computer interaction. Concatenating strings has always been essential since it allows for dynamic command and file manipulation.

One historical alternative is the `expr` command, which feels like a relic now:

```Bash
older_way=$(expr $greeting $name)
```

But Bash said, "Who needs that hassle?" and made it natural. How? Well, Bash treats strings like the cozy friends they are: put 'em side by side and they'll snuggle up together into one long string.

Under the covers, Bash handles this without any special function or syntax for concatenation. The words or variables just flow together. However, if your variables could start with a number or an underscore, you'd usually wrap them in curly braces to prevent confusion with other variable names.

There's a catch though: spaces matter. If you're not careful, you might end up with unintended gaps or a squished-together mess.

A current alternative is using the `printf` function, offering you more control over formatting:

```Bash
printf -v full_greeting "%s%s" "$greeting" "$name"
echo $full_greeting  # Outputs: Hello, world!
```

## See Also

- [GNU Bash manual](https://www.gnu.org/software/bash/manual/) for the nuts and bolts of all things Bash.
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/) for scripting gymnastics and more examples.
