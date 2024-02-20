---
date: 2024-01-20 17:56:40.503373-07:00
description: "Reading command line arguments lets your Python script play nice with\
  \ user inputs from the terminal. Why? Well, because flexibility is key; users can\u2026"
lastmod: 2024-02-19 22:05:18.229226
model: gpt-4-1106-preview
summary: "Reading command line arguments lets your Python script play nice with user\
  \ inputs from the terminal. Why? Well, because flexibility is key; users can\u2026"
title: Reading command line arguments
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments lets your Python script play nice with user inputs from the terminal. Why? Well, because flexibility is key; users can tweak behavior without editing your precious code.

## How to:

Using Python's `sys` module, you can snag those command line arguments easily. Here’s how to access them in your script:

```python
import sys

# First argument is always the script name, so we skip it
arguments = sys.argv[1:]

# Do something with the arguments
print("You entered:", arguments)
```

Run your script like this:

```bash
python your_script.py these are your arguments
```

Sample Output:

```
You entered: ['these', 'are', 'your', 'arguments']
```

## Deep Dive

Way back when, folks interacted with computers through command lines. That's why most languages, Python included, have a way to read command line arguments. It's how scripts were controlled before GUIs came along.

Python's `sys.argv` is handy, but for the fancier command-parsing dance, there's `argparse`. `argparse` is a module for when you need more than the basics – like when your arguments need names, types, or default values.

Now, `sys.argv` is just a list. Everything you pass is a string, no matter what. There's no magic; if you want numbers, convert them yourself with something like `int()` or `float()`.

## See Also

For more on `sys.argv` and `argparse`, check out the Python docs:

- `sys.argv`: https://docs.python.org/3/library/sys.html#sys.argv
- `argparse` tutorial: https://docs.python.org/3/howto/argparse.html 

And if you really want to dive head-first into command line interfaces:

- Click: https://click.palletsprojects.com/en/7.x/
- docopt: http://docopt.org/ 

Happy coding!
