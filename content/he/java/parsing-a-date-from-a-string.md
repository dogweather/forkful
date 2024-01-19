---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

זיקוי תאריך ממחרוזת הוא התרגום של מידע תאריך ממחרוזת לאובייקט Date. מתכנתים עושים את זה כאשר התאריך מגיע כמחרוזת ונדרש להמשיך עם החישובים או התמריצים.

## כיצד:

הנה הקוד מסוים עבודה איתו:

```Java
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class Main {
    public static void main(String[] args) {
        SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy");
        try {
            Date date = formatter.parse("23-10-2021");
            System.out.println(date);
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }
}
```
שימו לב לפלט:
```Java
Sat Oct 23 00:00:00 CEST 2021
```

## צוללים לעומק:

1) באופן היסטורי, מפענחי תאריכים ב- Java הופקו לשימוש בשעון מחשב בפורמט מחרוזת. זה היה נוח לשימוש בפרויקטים שבהם נדרשה התאמה לפורמטים שונים של תאריך.
2) מדובר במקרה המקובל, אך ניתן לאמת ולזיקב תאריכים בשפות תכנות אחרות או במקרים שונים. האפשרויות כוללות שימוש ב- LocalDate ו- DateTimeFormatter ב- Java 8 ועוד.
3) בפרט, המחלקות SimpleDateFormat ו- Date ב- Java מאפשרות לנו לזיקב תאריך ממחרוזת. SimpleDateFormat הוא קלאס שנוצר עם מחרוזת עבור הפורמט המיוחד שאנו רוצים, ומשמש לפענח מחרוזת לאובייקט מסוג Date.

## ראו גם:

היכן ניתן להשיג מידע נוסף:
1) [Oracle Docs - SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
2) [Baeldung - Java Date Format](https://www.baeldung.com/java-date)
3) [Stack Overflow - Parsing Strings to Dates](https://stackoverflow.com/questions/4216745/java-string-to-date-conversion).