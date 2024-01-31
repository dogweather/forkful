---
title:                "ניתוח תאריך ממחרוזת"
date:                  2024-01-20T15:36:45.314716-07:00
html_title:           "Arduino: ניתוח תאריך ממחרוזת"
simple_title:         "ניתוח תאריך ממחרוזת"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
Parsing dates means turning text into a `Date` object. We do this because dates in strings are not ready for operations like comparing or calculating time spans.

## איך לעשות:
```java
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateParsingExample {
    public static void main(String[] args) {
        String dateStr = "2023-04-05";
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");

        try {
            Date date = formatter.parse(dateStr);
            System.out.println(date); // Wed Apr 05 00:00:00 IDT 2023
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

## נסיעה לעומק:
Once upon a time, Java used `java.util.Date` and `SimpleDateFormat` for all date-time operations. These had issues: not thread-safe and kinda clunky. So, in came `java.time` in Java 8—better and thread-safe. Alternatives to parsing are many. You've got the old `Date` and `SimpleDateFormat`, or the newer, shinier `LocalDate`, `LocalDateTime`, and so on from `java.time`. When implementing, remember to handle exceptions (bad formats cause trouble) and always specify a timezone!

## ראה גם:
- The `java.time` package docs: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- SimpleDateFormat docs: [https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- More on thread safety with date-time classes: [https://www.baeldung.com/java-thread-safety](https://www.baeldung.com/java-thread-safety)
