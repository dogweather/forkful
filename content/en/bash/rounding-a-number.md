---
title:                "Rounding a number"
date:                  2024-01-24T20:57:57.538745-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding a number"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/rounding-a-number.md"
---

{{< edit_this_page >}}

## What & Why?
Rounding a number is the process of adjusting it to the nearest whole number or to a certain number of decimal places. Programmers round numbers to simplify figures, improve readability, and sometimes to meet the requirements of specific calculations that cannot handle too much precision.

## How to:

Rounding numbers in bash can be a bit tricky since bash doesn't support floating-point arithmetic out of the box. However, we can get the job done by using external tools like `bc`, a calculator language, or we can manipulate the numbers in creative ways. Here's how you can go about it:

```ProgLang.BASH
# Rounding to the nearest whole number using 'bc'
echo "scale=0; 3.14159/1" | bc
```
Output: `3`

```ProgLang.BASH
# Rounding to two decimal places using 'bc'
echo "scale=2; 3.14159/1" | bc
```
Output: `3.14`

Or, you can round numbers using `printf`:

```ProgLang.BASH
# Using printf to round to whole numbers
printf "%.0f\n" 3.14159
```
Output: `3`

```ProgLang.BASH
# Using printf to round to two decimal places
printf "%.2f\n" 3.14159
```
Output: `3.14`

## Deep Dive:
Before `bc` and `printf` came into play, rounding numbers was a complex task in bash. In the early days, you might have had to write your own functions or completely offload the math to another program.

Now though, `bc` and `printf` are widely available, but they work differently:

- `bc` evaluates expressions according to the specified scale; rounding is a side effect of adjusting the scale. 
- `printf`, on the other hand, is more akin to rounding in traditional programming languages because it formats the number to the specified number of decimal places.

Implementation wise, rounding numbers in bash typically involves converting the number to a string that represents the rounded value. It's crucial to understand that bash works best with integers and string manipulation. So when dealing with non-whole numbers, external tools come to the rescue.

Another alternative is using awk, a powerful text-processing tool, which does support floating-point arithmetic:

```ProgLang.BASH
# Using awk to round to the nearest whole number
echo 3.14159 | awk '{printf("%.0f\n", $1)}'
```
Output: `3`

Remember, each of these methods comes with its own set of constraints and limitations that you'll want to be aware of for complex scripts or precision-critical applications.

## See Also:

- [`bc` Manual](https://www.gnu.org/software/bc/manual/html_mono/bc.html)
- [GNU `awk` User's Guide](https://www.gnu.org/software/gawk/manual/gawk.html)
- [`printf` Syntax](https://ss64.com/bash/printf.html)