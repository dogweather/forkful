---
title:                "तिथि को स्ट्रिंग में परिवर्तित करना"
html_title:           "Java: तिथि को स्ट्रिंग में परिवर्तित करना"
simple_title:         "तिथि को स्ट्रिंग में परिवर्तित करना"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Date conversion is the process of changing a date from its original format to a string, or a sequence of characters. This is important for programmers because it allows us to manipulate and display dates in a more user-friendly way.

## How to:

Converting a date into a string in Java is a simple process. Let's take a look at a few examples.

```java
// Current date
Date date = new Date(); 

// Conversion using predefined format
DateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy"); 
String stringDate = dateFormat.format(date); 
System.out.println(stringDate); 
```

Output: 21/05/2021

```java
// Custom date format
SimpleDateFormat dateFormat2 = new SimpleDateFormat("dd MMMM, yyyy"); 
String stringDate2 = dateFormat2.format(date); 
System.out.println(stringDate2); 
```

Output: 21 May, 2021

## Deep Dive

Converting dates to strings was not always a simple task in programming. In the past, developers had to write their own code to manipulate and format dates. The introduction of the Java class SimpleDateFormat in Java 1.1 made this process much easier.

There are also alternatives to using SimpleDateFormat, such as the Calendar class which allows for more flexibility in date manipulation. However, SimpleDateFormat is still widely used due to its simplicity and ease of use.

When converting dates to strings, it's important to pay attention to the format string used. Any changes to the format can result in unexpected output or errors. It's also useful to know that the Date class in Java stores dates as a long integer representing milliseconds since January 1, 1970.

## See Also

To learn more about converting date to string in Java, check out the official documentation: https://docs.oracle.com/javase/8/docs/api/index.html