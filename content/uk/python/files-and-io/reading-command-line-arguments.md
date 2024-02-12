---
title:                "Читання аргументів командного рядка"
aliases:
- /uk/python/reading-command-line-arguments.md
date:                  2024-01-20T17:56:44.480306-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання аргументів командного рядка"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
## Що це таке та навіщо?

Reading command line arguments means grabbing the extra info you pass to your script when you run it. Programmers do this to make scripts interact with the user, customize their behavior, or handle different tasks without changing the code.

## How to:
## Як це зробити:

```python
import sys

# Simple script to echo the command line arguments
def main():
    # Check if we've got arguments besides the script name
    if len(sys.argv) > 1:
        for index, arg in enumerate(sys.argv[1:], start=1):
            print(f"Argument {index}: {arg}")
    else:
        print("No arguments were provided.")

if __name__ == "__main__":
    main()
```

Run it like this: `python your_script.py Hello World 123`
Sample output:

```
Argument 1: Hello
Argument 2: World
Argument 3: 123
```

## Deep Dive
## Поглиблений огляд

Historically, command line arguments come from the time before GUIs were common. They've stuck around because they're a simple and effective way to input data. Alternatives include using input from within the script, config files, or environment variables, but each has its use case. Python's `sys.argv` fetches arguments as a list, with the script name as the first element. Libraries like `argparse` offer more control, with parsing options, custom help messages, and more.

## See Also
## Див. також

- Python's `argparse` module: [docs.python.org/3/library/argparse.html](https://docs.python.org/3/library/argparse.html)
- An in-depth guide to command line arguments: [realpython.com/command-line-interfaces-python-argparse/](https://realpython.com/command-line-interfaces-python-argparse/)
- Using environment variables in Python: [12factor.net/config](https://12factor.net/config)
