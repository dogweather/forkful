---
title:    "Python recipe: Converting a date into a string"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Why

One common task in programming is converting a date into a string. This can be useful for displaying a date in a specific format or for saving dates in a certain format for future use. Understanding how to convert dates into strings is an essential skill for any Python programmer.

## How To

Converting a date into a string in Python is a relatively straightforward process. We will use the built-in `datetime` module to work with dates and times. First, we need to import the `datetime` module:

```python 
import datetime
```

Next, let's create a new `datetime` object using the `datetime()` constructor. We will pass in the year, month, and day as arguments.

```python 
my_date = datetime.datetime(2021, 9, 1)
```

Now, we can use the `strftime()` method to convert the date into a string in a specified format. The `strftime()` method takes in a format string as an argument. This format string specifies how the date should be displayed. Some commonly used format codes are:

- `%a`: abbreviated weekday name (e.g. Mon)
- `%A`: full weekday name (e.g. Monday)
- `%b`: abbreviated month name (e.g. Jan)
- `%B`: full month name (e.g. January)
- `%d`: day of the month (e.g. 01)
- `%m`: month (e.g. 09)
- `%Y`: year (e.g. 2021)

Here is an example of converting our `my_date` object into a string in the format of "Month Day, Year":

```python 
print(my_date.strftime("%B %d, %Y"))
```

This will output: "September 01, 2021". You can experiment with different format codes to achieve your desired output.

## Deep Dive

Behind the scenes, the `datetime` module uses the `strftime()` method to convert the date into a string. The `strftime()` method uses the locale's formatting conventions by default, but you can also provide a custom format string as shown in the example above. It also supports localization by using the locale module which enables us to display dates in different languages and date formats.

It is worth noting that in addition to the `strftime()` method, the `datetime` module also has a `strptime()` method which does the opposite - it converts a string into a structured date object. This can be useful when reading and parsing dates from a file or user input.

## See Also

- [Python official documentation on datetime module](https://docs.python.org/3/library/datetime.html)
- [Python strftime reference](https://strftime.org/)
- [Python strptime reference](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes)