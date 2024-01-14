---
title:    "Java recipe: Comparing two dates"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Why
As a Java programmer, you may come across scenarios where you need to compare two dates. This could be for various reasons such as checking if a certain date falls within a specific range or sorting a collection of objects based on their dates. Whatever the reason may be, knowing how to compare two dates in Java is a useful skill to have.

## How To

To compare two dates in Java, we will be using the `java.util.Date` class. Let's take a look at a simple example:

```Java
Date firstDate = new Date(); // current date
Date secondDate = new Date(2021, 10, 15); // custom date (November 15, 2021)
```
Using the `compareTo()` method of the `Date` class, we can compare the two dates and store the result in an integer variable. This method returns 0 if the dates are equal, a positive value if the first date is after the second date, and a negative value if the first date is before the second date.

```Java
int result = firstDate.compareTo(secondDate);
```

Let's print out the value of `result` to see the output:

```Java
System.out.println(result);
```

The output for this example should be -1, since the first date is before the second date. You can also use the `before()` and `after()` methods to check if a date is before or after another date, respectively.

## Deep Dive

While the above example shows a simple way to compare two dates, there are a few things to keep in mind when working with dates in Java.

- The `java.util.Date` class has been deprecated since Java 8, and the recommended way to work with dates is by using the new `java.time` API.
- The `compareTo()` method considers milliseconds as well, so if the dates have the same day, month, and year but different time values, the result will not be 0.
- When working with leap years, keep in mind that the `Date` class does not take leap years into account, so you may encounter unexpected results.

To overcome these limitations, you can also use libraries such as Joda-Time or the new `java.time` API to compare two dates in a more accurate and reliable manner.

## See Also

- [Comparing Two Dates in Java (Baeldung)](https://www.baeldung.com/java-compare-dates)
- [Java Date: compare dates using ‘compareTo()’](https://kodejava.org/java-date-compare-dates-using-compareto/)
- [How to Compare Dates in Java (GeeksforGeeks)](https://www.geeksforgeeks.org/how-to-compare-dates-in-java/)