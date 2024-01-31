---
title:                "Аналіз дати з рядка"
date:                  2024-01-20T15:36:44.133291-07:00
html_title:           "Arduino: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"

category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
**Що таке та навіщо?**

Parsing dates means converting text into a date format Java understands. Programmers do this to manipulate dates, such as calculating durations or comparing them.

## How to:
**Як це зробити:**

Let's say you've got a date as a string: "2023-03-21". You want Java to get this as a `LocalDate`. Here's how:

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateParsingExample {

    public static void main(String[] args) {
        String dateString = "2023-03-21";
        DateTimeFormatter formatter = DateTimeFormatter.ISO_DATE;
        LocalDate date = LocalDate.parse(dateString, formatter);
        
        System.out.println(date);
    }
}
```

Output:

```
2023-03-21
```

This will parse a string following the "YYYY-MM-DD" format. Easy!

## Deep Dive:
**Поглиблений аналіз:**

Back in the day, Java used `SimpleDateFormat` from the `java.text` package. It had issues: thread-unsafe and clunky. Then Java 8 happened in 2014, bringing `java.time`, the modern Java Date and Time API — way better.

Alternatives? Sure. Older projects might use Joda-Time or even the `java.util` date classes. But since Java 8, the best practice is `java.time`.

Implementation details? `DateTimeFormatter` is the main tool here. It's flexible — you can define your own pattern or use predefined ones. It's also immutable and thread-safe, which solves old problems.

## See Also:
**Дивіться також:**

- For all the date-time patterns: [DateTimeFormatter Docs](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- More about `java.time` package: [Oracle Tutorials](https://docs.oracle.com/javase/tutorial/datetime/)
- Specifics on `LocalDate`: [LocalDate Docs](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
