---
title:                "Parsing a date from a string"
html_title:           "Java recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

When working with dates in programming, sometimes we need to convert a string into a date data type. This process is called parsing a date from a string, and it allows us to manipulate dates in our code more easily. For example, we can compare two dates, add/subtract days, or format the date in a specific way. In short, parsing a date from a string is a useful tool for handling dates in our programs.

## How to:

Parsing a date from a string in Java is accomplished using the ```SimpleDateFormat``` class. Let's take a look at an example below:

```Java
String dateString = "October 31, 2020";
SimpleDateFormat format = new SimpleDateFormat("MMMM d, yyyy");
Date date = format.parse(dateString);
System.out.println(date);
```

In this example, we first declare a String variable called ```dateString```, which holds the date we want to parse. Then, we create a new ```SimpleDateFormat``` object and specify the format of the date string using a pattern string. In this case, "MMMM" refers to the full month name, "d" refers to the day of the month, and "yyyy" refers to the four-digit year. Finally, we call the ```parse()``` method on our ```format``` object which converts the string into a date and assigns it to the ```date``` variable. The output will be the date in a format based on the pattern we specified (in this case: "Sat Oct 31 00:00:00 EDT 2020").

We can also format a date into a string using the ```SimpleDateFormat``` class. Let's see an example:

```Java
Date date = new Date();
SimpleDateFormat format = new SimpleDateFormat("MM/dd/yyyy");
String dateString = format.format(date);
System.out.println(dateString);
```

This code will print out the current date in the format of "month/day/year". Note that we used the ```format()``` method in this example, which takes in a date object and returns a string based on the specified pattern.

## Deep Dive:

The concept of parsing a date from a string has been around since the early days of programming and has been present in multiple programming languages. In Java, before the introduction of the ```SimpleDateFormat``` class in Java 1.1, the ```DateFormat``` class was commonly used for parsing and formatting dates. However, the ```SimpleDateFormat``` class offers more flexibility and options for customizing the format.

There are also alternative ways to parse a date from a string in Java, such as using the ```DateTimeFormatter``` class introduced in Java 8. This class is thread-safe and offers more advanced features for handling dates and times. Additionally, there are third-party libraries like Joda-Time and Apache Commons Lang that provide date parsing and formatting utilities.

The implementation of parsing a date from a string in Java may seem straightforward, but it is important to keep in mind that handling dates and times can be complex due to various time zones, leap years, and daylight saving time adjustments.

## See Also:

- [Java SimpleDateFormat documentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java DateTimeFormatter documentation](https://docs.oracle.com/en/java/javase/13/docs/api/java.base/java/time/format/DateTimeFormatter.html)
- [Joda-Time library](https://www.joda.org/joda-time/)
- [Apache Commons Lang library](https://commons.apache.org/proper/commons-lang/)