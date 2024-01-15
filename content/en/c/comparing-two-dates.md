---
title:                "Comparing two dates"
html_title:           "C recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Date comparison is a common task in many programming projects. Whether you need to check if a certain date occurred before or after another date, or if two dates are equal, having a good understanding of how to compare dates is crucial for writing efficient and reliable code.

## How To

Comparing dates in C is a straightforward process. First, you will need to create two date variables using the `time_t` type. Then, you can use the `difftime()` function to calculate the difference between the two dates in seconds. Finally, you can use conditional statements or the `strcmp()` function to compare the result and determine the relationship between the two dates.

Here's an example of comparing two dates in C:

```C
#include <stdio.h>
#include <time.h>

int main() {
	time_t date1, date2;
	double difference;

	// Define the first date
	date1 = time(NULL); // current date and time

	// Define the second date
	date2 = time(NULL) - 86400; // 24 hours before the current date and time

	// Calculate the difference in seconds between the two dates
	difference = difftime(date1, date2);

	// Compare the result and print the appropriate message
	if (difference > 0) {
		printf("The first date occurs after the second date.\n");
	} else if (difference < 0) {
		printf("The first date occurs before the second date.\n");
	} else {
		printf("The two dates are equal.\n");
	}

	return 0;
}
```

The output of this code would be: `The two dates are equal.`

## Deep Dive

When comparing two dates, it's important to understand the underlying data type, `time_t`. This type represents the current time in seconds since the epoch (January 1, 1970). Therefore, when you use `time(NULL)` to get the current time, you are essentially getting the number of seconds that have passed since the epoch.

The `difftime()` function takes two `time_t` variables as parameters and returns the difference between them in seconds. In the example above, we subtracted 24 hours in seconds from the current time to get a difference of 86400 seconds.

Additionally, when comparing dates in C, it's important to consider leap years and daylight saving time. These can affect the number of days, hours, and seconds in a given time period and must be taken into account when comparing dates.

## See Also

- [C time.h documentation](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Using the time function in C](https://www.guru99.com/c-time-date.html)
- [Date comparison in C - GeeksforGeeks](https://www.geeksforgeeks.org/c-program-compare-two-dates/)