---
title:    "Java recipe: Calculating a date in the future or past"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past is a common task in many Java programs, especially in applications dealing with scheduling or time-sensitive data. By being able to accurately determine these dates, developers can provide a more robust and accurate experience for their users.

## How To

To calculate a date in the future or past, we can use the `Calendar` class in Java. First, we need to create an instance of `Calendar` and set it to the current date:

```Java
Calendar calendar = Calendar.getInstance();
```

Next, we can use the `add()` method to add or subtract a certain amount of time to our current date. For example, if we want to calculate the date 3 days in the future, we can use:

```Java
calendar.add(Calendar.DAY_OF_MONTH, 3);
```

This will add 3 days to our current date. Similarly, if we want to calculate the date 5 months in the past, we can use:

```Java
calendar.add(Calendar.MONTH, -5);
```

Lastly, to retrieve the calculated date, we can use the `getTime()` method and format it as desired.

```Java
Date calculatedDate = calendar.getTime();
SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");
String formattedDate = sdf.format(calculatedDate);
System.out.println("Calculated date: " + formattedDate);
```

This will output the calculated date in the format of "dd/MM/yyyy". Other formatting options can also be used, depending on the desired output.

## Deep Dive

Behind the scenes, the `Calendar` class uses the concept of milliseconds to add or subtract time. By default, it is set to the current date and time when we create an instance of `Calendar`. However, we can also set a specific date using the `set()` method:

```Java
calendar.set(Calendar.MONTH, 11); // Sets the month to December
calendar.set(Calendar.YEAR, 2020); // Sets the year to 2020
```

Additionally, the `add()` method allows us to select from a variety of fields such as days, months, weeks, years, and even minutes and seconds. This makes the `Calendar` class a versatile and essential tool for calculating dates in various scenarios.

## See Also

- [Java Calendar Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Java SimpleDateFormat Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java Date and Time API Tutorial](https://www.baeldung.com/java-date-time-api)