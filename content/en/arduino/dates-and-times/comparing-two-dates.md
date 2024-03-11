---
date: 2024-01-20 17:32:19.187930-07:00
description: "Comparing two dates means figuring out which one is earlier, later,\
  \ or if they're the same. Programmers do it to track time-based events, like scheduling\u2026"
lastmod: '2024-03-11T00:14:34.200606-06:00'
model: gpt-4-1106-preview
summary: "Comparing two dates means figuring out which one is earlier, later, or if\
  \ they're the same. Programmers do it to track time-based events, like scheduling\u2026"
title: Comparing two dates
---

{{< edit_this_page >}}

## What & Why?
Comparing two dates means figuring out which one is earlier, later, or if they're the same. Programmers do it to track time-based events, like scheduling tasks or logging data over time.

## How to:
In Arduino, you can compare dates using the `TimeLib.h` library. Install it first. Then, check out this snippet:

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  // Set up two different times (year, month, day, hour, minute, second)
  // Here we are setting 3rd March 2023, 8:30:00 and 4th March 2023, 16:45:00
  time_t firstTime = makeTime({0, 30, 8, 3, 3, 2023});
  time_t secondTime = makeTime({0, 45, 16, 4, 3, 2023});
  
  // Compare the two times
  if (firstTime < secondTime) {
    Serial.print("First time is earlier.");
  } else if (firstTime > secondTime) {
    Serial.print("Second time is earlier.");
  } else {
    Serial.print("Both times are the same.");
  }
}

void loop() {
  // Nothing here
}
```

Sample output:
```
First time is earlier.
```

## Deep Dive
Arduino doesn't have built-in support for date and time, so we use libraries like `TimeLib.h`. Before libraries, folks had to manually calculate and compare dates—tricky business due to leap years, different month lengths, and such.

Other ways to handle dates include RTC (Real Time Clock) modules, like the DS3231, which keep the time even when the Arduino is off. For comparing, you'd still pull the dates into your program and compare them just like we did above.

When implementing, account for time zones and daylight saving if needed. TimeLib can handle UTC time, which sidesteps these issues, but local times require extra care.

## See Also
- [TimeLib Library Documentation](https://www.pjrc.com/teensy/td_libs_Time.html) - Details on using the Time library.
- [Arduino Time Library](https://github.com/PaulStoffregen/Time) - The GitHub repository for the Time library.
