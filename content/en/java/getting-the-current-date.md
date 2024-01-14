---
title:                "Java recipe: Getting the current date"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
In the world of programming, time and date are crucial elements that need to be continuously monitored and accurately recorded. Whether you are working on a calendar application, a scheduling algorithm, or simply want to display the current date on your program, getting the current date is an essential task. Thankfully, Java provides us with a simple and efficient way to retrieve the current date using its built-in library.

## How To
Getting the current date in Java is as easy as 1-2-3, literally. First, import the necessary libraries, specifically the `java.time` library. Then, create an object of the `LocalDate` class using the `now()` method to get the current date. This object will hold the current date value, which can then be formatted and displayed as desired. Let's take a look at the code below:

```Java
import java.time.LocalDate;

public class Main {
  public static void main(String[] args) {
    // create an object of the LocalDate class using now() method
    LocalDate currentDate = LocalDate.now();

    // display current date in ISO format
    System.out.println("Today's date is: " + currentDate);
  }
}
```

The above code will output: "Today's date is: 2021-03-22". You can also format the date in any pattern you prefer using the `DateTimeFormatter` class. For example, if you want to display the date in the format "March 22, 2021", you can use the following code:

```Java
// format the date
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("MMMM dd, yyyy");

// display formatted date
System.out.println("Today's date is: " + currentDate.format(formatter));
```

The output will then be: "Today's date is: March 22, 2021". Easy, right?

## Deep Dive
In the `LocalDate` class, there is a `now()` method and a `now()` method that accepts an argument for the specific timezone. These methods return the current date in the system's timezone. If you want to get the current date in a different timezone, you can use the `of()` method and specify the timezone. Additionally, the `LocalDate` class also has other useful methods such as `plus()` and `minus()` for adding and subtracting days, months, and years to a date object.

Another important thing to note is that the `LocalDate` class is immutable, meaning you cannot modify the value of the date object. If you want to perform any modification, you need to create a new `LocalDate` object with the new value.

## See Also
- Oracle's official documentation on [LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- Tutorialspoint's article on [Java - Get Current Date and Time](https://www.tutorialspoint.com/java/java_date_time.htm)
- GeeksforGeeks guide on [Java LocalDate Class](https://www.geeksforgeeks.org/java-localdate-class/)

Getting the current date in Java might seem like a mundane task, but it is a fundamental skill that every programmer should have. With the help of the `java.time` library, you can easily incorporate time and date functions into your programs. Happy coding!