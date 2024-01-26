---
title:                "Working with complex numbers"
date:                  2024-01-25T02:59:42.293888-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with complex numbers"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Complex numbers consist of a real part and an imaginary part. Programmers use them in fields like signal processing, quantum mechanics, and whenever the calculation requires them, because normal real numbers just don't cut it.

## How to:
Bash doesn't support complex numbers natively. You'll often use an external tool like `bc` with its `-l` option. Here's how you crunch complex numbers in bash:

```bash
echo "sqrt(-1)" | bc -l
```

Output:
```bash
j
```

Multiplication:

```bash
echo "(-1 + -1i) * (4 + 3i)" | bc -l
```

Output:
```bash
-1.00000000000000000000-7.00000000000000000000i
```

## Deep Dive
Complex numbers have been around since the 16th century, but scripting languages like Bash are not primed for mathematical computations like complex numbers out of the box. That’s why `bc` or other tools like `awk` often come into play. Some alternative languages for working with complex numbers are Python with its `cmath` module and MATLAB, which are both built for more advanced mathematical functions. As for Bash, it's all about leveraging tools - `bc` uses the lowercase 'i' to represent the imaginary unit and supports basic operations like addition, subtraction, multiplication, and division.

## See Also
- The `bc` manual: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- GNU Octave (alternative for MATLAB): https://www.gnu.org/software/octave/
- Python `cmath` module: https://docs.python.org/3/library/cmath.html
