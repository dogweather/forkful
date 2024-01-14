---
title:                "C recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

You may be wondering why someone would want to compare two dates in a C programming language. Well, dates are a crucial component in many applications and being able to compare them can help with tasks like sorting data or checking for overlaps in schedules. In this blog post, we will discuss how to compare two dates in C and dive deeper into the process.

## How To

To start, let's declare two variables to hold our dates. We will use a structure called "date" that has three integer members: day, month, and year.

```C
struct date {
  int day;
  int month;
  int year;
};

// Initializing two variables, date1 and date2
struct date date1 = {28, 7, 2021};
struct date date2 = {12, 6, 2021};

// Comparing dates using if statements
if (date1.year > date2.year) {
  printf("date1 is later than date2");
}
else if (date1.year < date2.year) {
  printf("date1 is earlier than date2");
}
else { // if years are equal
  if (date1.month > date2.month) {
    printf("date1 is later than date2");
  }
  else if (date1.month < date2.month) {
    printf("date1 is earlier than date2");
  }
  else { // if months are equal
    if (date1.day > date2.day) {
      printf("date1 is later than date2");
    }
    else if (date1.day < date2.day) {
      printf("date1 is earlier than date2");
    }
    else { // if days are equal
      printf("Date1 and date2 are the same");
    }
  }
}
```

Let's break down the logic behind this code. We are using three if statements to compare the years, months, and days of the two dates. If one date has a greater value in any category, it is considered to be later or earlier than the other date. If all values are equal, then the dates are the same.

Here is the expected output for our two sample dates:

```C
date1 is later than date2
```

You can also use this logic to compare two dates using different variables, such as the current date and a user-inputted date.

## Deep Dive

Now let's dive deeper into comparing two dates in C. The key concept to understand is that dates can be converted into an integer value using a specific formula. This allows for easier comparison since integers can be easily compared with primitive operators like < and >.

The formula for converting a date into an integer value is as follows:

```
integer date value = (year * 10000) + (month * 100) + day;
```

For example, if we have a date of July 28, 2021, the integer value would be calculated as:

```
2021 * 10000 = 20210000
7 * 100 = 700
20210000 + 700 = 20210700
20210700 + 28 = 20210728
```

This produces a unique integer value for each date, allowing for easy comparison.

## See Also

Now that you know how to compare two dates in C, you can explore more about dates and time in the C programming language. Here are some helpful links:

- [Date and Time Functions in C](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Date and Time in C Programming Language](https://www.geeksforgeeks.org/c-programming-language/#Dates-and-Time)
- [Handling Date and Time in C Language](https://www.includehelp.com/c-programming-questions/handling-date-and-time-in-c-programming-language.aspx)