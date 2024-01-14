---
title:    "Java: המרת תאריך למחרוזת"
keywords: ["Java"]
---

{{< edit_this_page >}}

## למה

למה אנשים מעוניינים להמיר תאריך למחרוזת בשפת ג'אווה?

## כיצד לעשות זאת

המרת תאריך למחרוזת בשפת ג'אווה ניתן לבצע באמצעות השיטות הקיימות במחלקת `SimpleDateFormat`. לדוגמה, ניתן להמיר תאריך למחרוזת באמצעות המתודה `format()` כדי לקבל תבנית רצויה. הנה דוגמא של המרת תאריך למחרוזת בפורמט `dd/MM/yyyy`:

```Java
SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
Date date = new Date();
String dateString = dateFormat.format(date);
System.out.println(dateString);
```

הפלט יהיה `24/12/2020`.

בנוסף, ניתן להשתמש במודפס ספציפיים לתאריכים כגון `dd` ליום, `MM` לחודש, ו`yyyy` לשנה. ישנן גם אפשרויות ליצור פורמטים אישיים עם השימוש בתבניות ירוקות וצהובות.

## חפירה עמוקה

המרת תאריך למחרוזת בשפת ג'אווה כוללת מספר שימושי של `SimpleDateFormat`. בנוסף, ישנן מחלקות נוספות כמו `DateFormat` ו-`DateTimeFormatter` המאפשרות גם המרת תאריך למחרוזת עם אפשרויות נוספות ופורמטים מתקדמים. אתם מוזמנים לחפש ולנסות את האפשרויות השונות כדי למצוא את הפתרון המתאים לכם.

## ראו גם

- [תיעוד רשמי של `SimpleDateFormat`](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [מדריך מפורט להמרת תאריך למחרוזת בשפת ג'אווה](https://www.baeldung.com/java-convert-date-to-string)