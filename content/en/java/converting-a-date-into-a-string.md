---
title:    "Java recipe: Converting a date into a string"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

As a Java programmer, you may come across situations where you need to convert a date object into a string. This is a common task when dealing with data formats, databases, and user input. By converting a date into a string, you can easily manipulate and present the date in a readable format.

## How To

Converting a date into a string in Java is a simple process that involves using the `SimpleDateFormat` class. Let's take a look at an example of converting today's date into a string:

```Java
// Import the necessary classes
import java.text.SimpleDateFormat;
import java.util.Date;

// Create a date object
Date today = new Date();

// Create a SimpleDateFormat object with the desired format
SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");

// Use the format() method to convert the date into a string
String dateString = dateFormat.format(today);

// Output the converted string
System.out.println(dateString);
```

The code above will output today's date in the format of `MM/dd/yyyy`, which stands for month, day, and year. You can change the format to fit your needs by adjusting the pattern in the `SimpleDateFormat` constructor.

Another useful feature of the `SimpleDateFormat` class is that it can also parse a string into a date object. Let's see how this works:

```Java
// Create a string representing a date
String dateStr = "12/25/2021";

// Use the parse() method to convert the string into a date object
Date christmas = dateFormat.parse(dateStr);

// Output the parsed date
System.out.println(christmas);
```

The `parse()` method takes in a string and returns a date object. It's important to note that the string must match the specified format in the `SimpleDateFormat` constructor, otherwise it will throw an exception.

## Deep Dive

Behind the scenes, the `SimpleDateFormat` class uses a pattern to define the format of the date. Some common patterns include `MM` for month, `dd` for day, `yyyy` for year, `hh` for hours, and `mm` for minutes. You can also use symbols such as `'-'` or `'/'` as separators.

It's also worth mentioning that the `SimpleDateFormat` class is not thread-safe, meaning it should not be shared among multiple threads. If you need to use it in a multi-threaded environment, you should use the `ThreadLocal` class to ensure thread safety.

## See Also

Here are some helpful resources for further learning and understanding:

- [Java SimpleDateFormat API](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [Java ThreadLocal API](https://docs.oracle.com/javase/7/docs/api/java/lang/ThreadLocal.html)
- [Java Date and Time Tutorial](https://www.baeldung.com/java-dates)

Now that you know how to convert a date into a string in Java, you can confidently handle date and time data in your projects. Happy coding!