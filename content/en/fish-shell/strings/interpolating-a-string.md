---
date: 2024-01-20 17:50:35.921519-07:00
description: 'How to: In Fish, you use double quotes and place the variable or command
  you want to interpolate with a dollar sign `$` straight in the string.'
lastmod: '2024-03-13T22:45:00.459541-06:00'
model: gpt-4-1106-preview
summary: In Fish, you use double quotes and place the variable or command you want
  to interpolate with a dollar sign `$` straight in the string.
title: Interpolating a string
weight: 8
---

## How to:
In Fish, you use double quotes and place the variable or command you want to interpolate with a dollar sign `$` straight in the string.

```fish
set name "world"
echo "Hello, $name!"
```

Output:
```
Hello, world!
```

To include the output of a command within a string:

```fish
echo "I have (count (ls)) files in this directory."
```

Output might be:
```
I have 9 files in this directory.
```

Variables and commands get evaluated and neatly tucked into the place you put them.

## Deep Dive
Before Fish and other modern shells, you'd often use a more clunky combo of quotes and concatenation—or rely on external tools—to get variables into strings.

In bash, for example, it'd look like this:

```bash
name="world"
echo "Hello, "$name"!"
```

Not as slick, right?

Fish not only streamlines this process but also handles errors more gracefully. If a variable doesn't exist, Fish will insert an empty string, lessening the chance of a crash from mishandled interpolations.

Alternatives to direct interpolation include using the `printf` command:

```fish
set animal "narwhal"
printf "The %s is an awesome creature!" $animal
```

Output:
```
The narwhal is an awesome creature!
```

In this case, `%s` is a placeholder for the string variable `$animal` that gets replaced by `printf`.

In terms of implementation, when Fish processes the command line, it parses the double-quoted strings and swaps the variables with their values on the fly. It's elegant and mimics the variable interpolation found in higher-level languages like Ruby or PHP.

## See Also
For more on Fish string manipulation and scripting, check these out:

- [Fish Shell Documentation: Quotes](https://fishshell.com/docs/current/index.html#quotes)
- [Fish Shell Tutorial](https://fishshell.com/docs/current/tutorial.html)
- [Stack Overflow: How to use variables in a command in Fish](https://stackoverflow.com/questions/2763006/how-to-use-variables-in-a-command-in-fish)
