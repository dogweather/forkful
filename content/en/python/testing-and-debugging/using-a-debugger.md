---
date: 2024-01-25 20:50:36.334976-07:00
description: 'How to: Let''s break down using `pdb`, Python''s built-in debugger.
  Imagine a file, `buggy.py`, with a sneaky bug.'
lastmod: '2024-03-13T22:44:59.712228-06:00'
model: gpt-4-1106-preview
summary: Let's break down using `pdb`, Python's built-in debugger.
title: Using a debugger
weight: 35
---

## How to:
Let's break down using `pdb`, Python's built-in debugger. Imagine a file, `buggy.py`, with a sneaky bug:

```Python
def add_one(number):
    result = number ++ 1
    return result

print(add_one(7))
```

Running this script, you expect `8`, but it just throws a syntax error. It's debugger time!

In your terminal, run:
```bash
python -m pdb buggy.py
```

You’ll enter the debugger, and it looks like this:
```Python
> /path_to_file/buggy.py(1)<module>()
-> def add_one(number):
```

Use `l(ist)` to see more code, `n(ext)` to go to the next line, or `c(ontinue)` to keep running the script. When you hit the error, `pdb` will stop and let you inspect.

After you correct `number ++ 1` to `number + 1`, restart the debugger to test the fix.
Remember, friends don't let friends code without a net. 'Nuff said.

## Deep Dive
Back in the Dark Ages of programming (a.k.a. before integrated development environments, or IDEs, were everywhere), debuggers were often stand-alone tools you’d use outside your text editor. They came to the rescue by letting programmers inspect the state of their software at various execution points.

As of 2023, Python's `pdb` isn't the only game in town. Folks might use IDEs like PyCharm or Visual Studio Code, which have their own slick debuggers baked in. These add handy features like breakpoints that you can set with a click, rather than typing cryptic commands.

Then there’s `ipdb`, a pip-installable package that brings the `IPython` goodness to debugging. It’s like `pdb` on performance enhancers, with tab completion and syntax highlighting.

Debuggers also vary in their implementation. Some get up close and personal with program execution at the machine or byte code level. Others, like many high-level language debuggers, run the code in a special environment that monitors variable states and controls execution flow.

## See Also
For the full scoop on Python’s own debugger, check out:
- The `pdb` documentation: https://docs.python.org/3/library/pdb.html

If you're curious about alternatives, these links will serve you well:
- `ipdb` repository and usage guide: https://github.com/gotcha/ipdb
- Debugging with Visual Studio Code: https://code.visualstudio.com/docs/python/debugging
- PyCharm debugging features: https://www.jetbrains.com/help/pycharm/debugging-code.html

Happy bug hunting!
