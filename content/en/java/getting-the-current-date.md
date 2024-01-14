---
title:    "Java recipe: Getting the current date"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Why 

As a Java programmer, you may wonder why it is important to know how to get the current date. The current date is a crucial piece of information in many applications, such as creating time stamps, scheduling tasks, and displaying real-time data. It is also useful for tracking and organizing data by date.

# How To

To get the current date in Java, we can use the ``` java.util.Date``` class. This class provides methods to get the current date, time, and both together. Let's take a look at some examples:

```
import java.util.Date;

// Create an instance of the Date class
Date currentDate = new Date(); 

// The toString() method returns the current date and time
System.out.println(currentDate.toString()); 

// The getTime() method returns milliseconds since January 1, 1970
System.out.println(currentDate.getTime()); 

// The getDate() method returns the current day of the month
System.out.println(currentDate.getDate()); 
```

Output:
```
Thu Oct 14 10:45:22 EDT 2021
1634210722869
14
```

The ```Date``` class also allows us to format the date in a specific way using ```SimpleDateFormat```. Let's see an example:

```
import java.util.Date;
import java.text.SimpleDateFormat;

// Create an instance of the Date class
Date currentDate = new Date();

// Create a SimpleDateFormat object with desired format
SimpleDateFormat dateFormat = new SimpleDateFormat("MM-dd-yyyy");

// Format the current date and store it in a String variable
String formattedDate = dateFormat.format(currentDate);

// Display the formatted date
System.out.println(formattedDate);
```

Output: 
```
10-14-2021
```

# Deep Dive

Java's ```Date``` class has been around since Java's early versions, but it has some limitations, including its handling of time zones and its mutable behavior. For these reasons, Java introduced the ```LocalDate``` class in Java 8, which offers more flexibility and accuracy in dealing with dates.

The ```LocalDate``` class belongs to the ```java.time``` package and offers methods to get the current date, format it, manipulate it, and perform calculations. Here's an example:

```
import java.time.LocalDate;

// Create an instance of the LocalDate class
LocalDate currentDate = LocalDate.now();

// Get the current date in ISO format
System.out.println(currentDate); 

// Format the current date and store it in a String variable
String formattedDate = currentDate.format(DateTimeFormatter.ofPattern("MM-dd-yyyy"));

// Display the formatted date
System.out.println(formattedDate); 

// Add one week to the current date
LocalDate oneWeekLater = currentDate.plusWeeks(1);

// Display the date one week later
System.out.println(oneWeekLater);  
```

Output:
```
2021-10-14
10-14-2021
2021-10-21
```

# See Also
- [Java Date class documentation](https://docs.oracle.com/javase/7/docs/api/java/util/Date.html)
- [Java LocalDate class documentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [SimpleDateFormat class documentation](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)