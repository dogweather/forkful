---
title:                "Java recipe: Converting a date into a string"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting a date into a string may seem like a simple task, but it can actually be quite useful in various programming scenarios. This allows us to easily manipulate and display dates in a human-readable format, making our code more user-friendly.

## How To

To convert a date into a string in Java, there are a few steps we need to follow. Let's take a look at an example code:

```Java
// Create a Date object
Date date = new Date();

// Create a SimpleDateFormat object with desired format 
// "yyyy-MM-dd HH:mm:ss" is a common format, but you can choose any format you want to display
SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

// Use the format() method to convert the date into a string
String dateString = format.format(date);

// Print the converted date string
System.out.println(dateString);
```

The output of the code will be something like this: "2021-10-01 10:30:45". As you can see, we have successfully converted the date into a string using the desired format.

Now let's say we want to convert a string into a date. We can do that by using the parse() method of the SimpleDateFormat class. Let's take a look at the code:

```Java
// Create a String representing a date 
String stringDate = "2021-10-01 10:30:45";

// Use the parse() method to convert the string into a date object
Date date = format.parse(stringDate);

// Print the converted date object
System.out.println(date);
```

The output will be: "Fri Oct 01 10:30:45 EDT 2021". As you can see, the string has been successfully converted into a date object.

## Deep Dive

Now that we have seen how to convert a date into a string and vice versa, let's take a deeper dive into the SimpleDateFormat class. This class allows us to specify a pattern for formatting and parsing dates. Some commonly used patterns are:

- "yyyy-MM-dd" for dates in the format of year-month-day
- "dd/MM/yyyy" for dates in the format of day/month/year
- "HH:mm:ss" for time in the format of hour:minute:second
- "EEEE" for day of the week in full name (i.e. Monday, Tuesday)

There are plenty more patterns available for different date and time formats. You can also customize your own pattern by using a combination of letters and symbols.

## See Also

- [Java SimpleDateFormat Documentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java Date Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [GeeksforGeeks: Convert Date to String in Java](https://www.geeksforgeeks.org/date-to-string-conversion-in-java/)
- [Baeldung: Date and Time in Java](https://www.baeldung.com/java-date-time)