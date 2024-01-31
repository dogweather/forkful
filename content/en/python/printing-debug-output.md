---
title:                "Printing debug output"
date:                  2024-01-20T17:53:05.673374-07:00
model:                 gpt-4-1106-preview
simple_title:         "Printing debug output"

category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Printing debug output is like having a conversation with your code to figure out what it's thinking. Programmers do it to track down gremlins causing mischief in their programs.

## How to:
Plain and simple, you print stuff to see what's going on. Here's the classic:

```Python
print("Hello, bug hunters!")
```

Feel like a detective yet? Now, let's see how your variables are behaving:

```Python
buggy_number = 42
print(f"Debug: The number is {buggy_number}")
```

When things get complex, you might peek into a list:

```Python
buggy_list = [1, 2, 3]
print(f"Debug: The list contains {buggy_list}")
```

Run these snippets, and your output is this:

```
Hello, bug hunters!
Debug: The number is 42
Debug: The list contains [1, 2, 3]
```

## Deep Dive
Debugging by printing has a long pedigree, going all the way back to when dinosaurs roamed the earth (also known as the early days of computing). It's simple and universally applicable because it just outputs whatever you want to check.

While `print()` is the quick-and-dirty tool in Python, alternatives exist. For real sleuthing, you might want to use logging with different levels like DEBUG, INFO, WARNING, etc. This way, you can control what gets printed and what gets silenced.

Sometimes, you'll hear about fancy debuggers which let you stop time (sort of) and snoop around your code as it runs. They're super powerful and worth learning, but don't let them make you feel bad for tossing in a quick `print()` here and there.

As for implementation, the simplicity of `print()` is its beauty. Just remember that constantly printing to the console can slow you down if you're doing it a zillion times in a loop. And, it can get messy super fast. Comments or removes those lines once you've nailed those bugs.

## See Also
For more on printing and debugging in Python:
- Python's built-in `print()` function: [Python docs on print](https://docs.python.org/3/library/functions.html#print)
- Python Logging: [Logging HOWTO](https://docs.python.org/3/howto/logging.html)
- For the debugger lovers: [Python docs on pdb](https://docs.python.org/3/library/pdb.html)
