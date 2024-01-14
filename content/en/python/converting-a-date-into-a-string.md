---
title:    "Python recipe: Converting a date into a string"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting a date into a string is a common task for many Python programmers. It allows us to format dates in a way that is easily readable for both humans and machines. Whether you are working with date data in a project or just looking to learn more about date manipulation in Python, understanding how to convert a date into a string is a useful skill to have.

## How To

Converting a date into a string can be achieved using the `strftime` function from the `datetime` module. Let's take a look at a few examples of how to use this function.

```Python
# Import the necessary modules
from datetime import datetime

# Create a date object
date = datetime(2021, 10, 31)

# Convert the date into a string using the default format
print(date.strftime("%m/%d/%Y"))
# Output: 10/31/2021

# Convert the date into a string using a custom format
print(date.strftime("%b %d, %Y"))
# Output: Oct 31, 2021
```

In the first example, we used the `%m/%d/%Y` format to display the date in the format of month/day/year. In the second example, we used the `%b %d, %Y` format to display the date in the format of abbreviated month name, day, and year.

There are many different formatting options available for the `strftime` function, so be sure to check the [documentation](https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior) for a full list of options.

## Deep Dive

Behind the scenes, the `strftime` function uses a combination of strftime directives and the current locale to convert the date into a string. The strftime directives dictate how the date will be formatted, while the locale determines the language and cultural conventions to be used in the conversion.

If no locale is specified, the conversion will use the default locale for your system. However, you can also explicitly specify a locale for more precise control over the conversion.

## See Also

- [Python datetime documentation](https://docs.python.org/3/library/datetime.html)
- [Python strftime directives](https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior)
- [Python locale documentation](https://docs.python.org/3/library/locale.html)