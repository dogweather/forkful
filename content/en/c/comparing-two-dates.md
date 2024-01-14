---
title:    "C recipe: Comparing two dates"
keywords: ["C"]
---

{{< edit_this_page >}}

## Why

When working with dates in C programming, it may be necessary to compare two dates in order to determine the order or duration between them. This can be useful for tasks such as scheduling events, calculating interest rates, or sorting data.

## How To

To compare two dates in C, you can use the `difftime()` function from the `time.h` header file. This function takes in two `time_t` variables, which represent the number of seconds since January 1, 1970, and calculates the difference between them in seconds. The syntax for using the `difftime()` function is as follows:

```
time_t date1, date2;
double difference;

difference = difftime(date1, date2);

printf("The difference between date1 and date2 is %f seconds\n", difference);
```

The output from this code will be the difference in seconds between the two dates. Keep in mind that both `date1` and `date2` must be valid `time_t` variables obtained from functions like `time()` or `localtime()`. Additionally, the `difftime()` function also works for comparing dates in the past or future.

## Deep Dive

The `difftime()` function uses the `time_t` variable type, which represents the number of seconds since the Unix Epoch. This value is calculated by subtracting the starting date (January 1, 1970) from the given date. This means that the `difftime()` function is not limited to comparing dates within the same year. It can handle dates from any year, as long as they are valid `time_t` values.

Furthermore, the `difftime()` function also takes into account leap years and daylight saving time. This ensures that the result of the comparison is accurate, even when dealing with dates that fall during these time transitions.

## See Also

For more information on working with dates in C, check out these resources:

- [time.h - C Reference](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Date and Time Programming in C](https://www.geeksforgeeks.org/date-time-programming-c/)
- [The function `difftime()` in C](https://www.techiedelight.com/difftime-function-c/)