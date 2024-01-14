---
title:                "C++ recipe: Calculating a date in the future or past"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
Calculating a date in the future or past can be a useful tool for various applications such as scheduling events, calculating deadlines, or even creating a countdown timer.

## How To

To calculate a date in the future or past, the first step is to select a starting date. This can be done by creating a `struct` in C++ that holds the necessary information for the date, such as the day, month, and year.

```C++
// Creating a struct for the date
struct Date {
  int day;
  int month;
  int year;
};
```

Next, we will need to define two functions, one for adding days to a given date and another for subtracting days. These functions will take in the `Date` struct as a parameter and return a new `Date` struct with the updated date.

```C++
// Function to add days to a date
Date addDays(Date currentDate, int daysToAdd) {
    // Logic to add days to the current date
    // ...
    return updatedDate;
}

// Function to subtract days from a date
Date subtractDays(Date currentDate, int daysToSubtract) {
    // Logic to subtract days from the current date
    // ...
    return updatedDate;
}

```

To calculate a future date, we can simply call the `addDays()` function with the desired number of days to add to the starting date.

```C++
// Calculating a future date
Date startingDate = {15, 5, 2021};
Date futureDate = addDays(startingDate, 10);  // Adding 10 days to the starting date

// Output: futureDate = {25, 5, 2021}
```

On the other hand, to calculate a past date, we can call the `subtractDays()` function with the desired number of days to subtract from the starting date.

```C++
// Calculating a past date
Date startingDate = {25, 5, 2021};
Date pastDate = subtractDays(startingDate, 10);  // Subtracting 10 days from the starting date

// Output: pastDate = {15, 5, 2021}
```

## Deep Dive

While the above approach works for basic calculations, there are some factors to consider when calculating dates in the future or past. These include leap years, differing number of days in each month, and adjusting for the end of a month or year.

To account for leap years, we can use the `year` value in our `Date` struct to determine if the current year is a leap year. If it is, we need to account for the extra day in February.

Additionally, we need to consider the varying number of days in each month. To do this, we can use a switch statement to check the `month` value in our `Date` struct and adjust the days accordingly.

Lastly, when calculating a date in the past, we need to be mindful of adjusting for the end of a month or year. For example, if we were to subtract 10 days from February 29, we would end up with February 19 instead of February 19 in a non-leap year.

By taking these factors into account and making necessary adjustments in our functions, we can ensure more accurate date calculations.

## See Also

- [Check Date Calculations in C++](https://www.codeproject.com/Articles/8222/Check-date-validity-logic-with-C)
- [Date and Time in C++](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)
- [Date and Time Functions in C++](https://www.geeksforgeeks.org/date-and-time-functions-in-cpp/)