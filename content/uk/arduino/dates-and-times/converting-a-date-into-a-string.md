---
date: 2024-01-20 17:35:49.262097-07:00
description: ''
lastmod: '2024-03-11T00:14:23.607919-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A"
---

{{< edit_this_page >}}

## What & Why?
## Що і Чому?
Date to string conversion means transforming a date (like year, month, day) into a text format. Programmers do this to display dates on screens, log events, or format data for storage.

## How to:
## Як це зробити:
Arduino doesn't have a built-in date type, but you can use libraries like `RTClib` for real-time clocks or `TimeLib` for time functions. Here's how you do it with `TimeLib`:

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(23, 59, 30, 12, 31, 2020); // Set the time to December 31, 2020, 23:59:30
}

void loop() {
  char buffer[20];
  sprintf(buffer, "%02d/%02d/%04d %02d:%02d:%02d", day(), month(), year(), hour(), minute(), second());
  Serial.println(buffer); // Outputs "31/12/2020 23:59:30"
  delay(1000); // Wait for a second
}
```

Sample output:
```
31/12/2020 23:59:30
```

## Deep Dive
## Детальніше
Originally, Arduino lacked time and date handling until libraries like `TimeLib` filled the gap. Alternatives like `RTClib` interface directly with real-time clock hardware. For converting date to string, `sprintf` is your go-to in C++, which Arduino uses. It formats data into a buffer. Careful with buffer sizes—they must be large enough to hold your string, including the terminating null character `\0`.

## See Also
## Дивіться також
- Arduino `TimeLib` library: https://github.com/PaulStoffregen/Time
- Arduino `RTClib` library by Adafruit: https://github.com/adafruit/RTClib
- `sprintf` reference: http://www.cplusplus.com/reference/cstdio/sprintf/
