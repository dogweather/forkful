---
title:    "Python recipe: Printing debug output"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why
We've all been there - spending hours or even days debugging our code, trying to pinpoint where the error lies. Sometimes we just can't figure it out on our own, and that's where printing debug output comes in handy. By printing out specific values or steps in our code, we can get a better understanding of what is happening and therefore, find the source of the issue more quickly.

## How To
To print debug output in Python, we can use the `print()` function. Let's say we have a function that is supposed to calculate the average of a list of numbers. But for some reason, it's not giving us the correct answer. We can use `print()` to see what's going on inside the function.

```Python
def calculate_average(numbers):
    total = 0
    for num in numbers:
        total += num
    average = total / len(numbers)
    return average

nums = [5, 10, 15]
print(calculate_average(nums))
```

Running the code above will output `10.0`, which is the incorrect average of the numbers in our list. Now let's add some `print()` statements to see what values our variables are taking on during each iteration of the loop.

```Python
def calculate_average(numbers):
    total = 0
    for num in numbers:
        print("Current total:", total)
        print("Current number:", num)
        total += num
    average = total / len(numbers)
    return average

nums = [5, 10, 15]
print(calculate_average(nums))
```

This time, we will get a more detailed output that shows us the value of `total` and `num` in each iteration of the loop. This can help us pinpoint where the issue lies - maybe we are not adding the numbers correctly or our list is not being read properly.

```
Current total: 0
Current number: 5
Current total: 5
Current number: 10
Current total: 15
Current number: 15
10.0
```

By adding `print()` statements strategically, we have successfully debugged our code and found the issue.

## Deep Dive
There are a few different ways we can use the `print()` function for debugging purposes. We can print out variables, steps in our code, or even specific messages to give us more information. Additionally, we can format our output using string formatting or the `repr()` function to get more detailed information about our variables.

For more advanced debugging, we can also use the `logging` module in Python, which gives us more control over what we want to print and when. This can be particularly useful for larger projects and complex code.

## See Also
For more information on printing debug output, check out these resources:

- [Real Python - Using print statements for debugging](https://realpython.com/python-print/)
- [Python Docs - Logging](https://docs.python.org/3/library/logging.html)
- [Python Debugging Techniques](https://www.safaribooksonline.com/library/view/python-debugging-techniques/0596000472/ch01.html)