---
title:                "להשוות שתי תאריכים"
html_title:           "Java: להשוות שתי תאריכים"
simple_title:         "להשוות שתי תאריכים"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה

כשאנו פותחים אפליקציות תוכנה, חווים עתים קרובים הצורך להשוות בין שתי תאריכים. השוואה זו מאפשרת לנו לבדוק אם תאריך מסוים קודם או אחרי תאריך אחרון, ולפעמים אפילו לחשב את הפרש זמן ביניהם. במאמר זה נלמד כיצד לבצע השוואה בין שתי תאריכים בשפת ג'אווה ונתנה דוגמאות מעשיות.

## איך לבצע השוואת תאריכים בשפת ג'אווה?

כדי לבצע השוואה בין שתי תאריכים בשפת ג'אווה נעשה שימוש ב- `LocalDate` ו- `ChronoUnit` מהחבילה `java.time`. נתחיל עם דוגמה פשוטה שמציגה איך לבדוק אם תאריך הוא קודם או אחרי תאריך אחר באמצעות הפעולה `isBefore` ו- `isAfter` בהתאמה.

```java
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

LocalDate firstDate = LocalDate.of(2021, 9, 1);
LocalDate secondDate = LocalDate.of(2021, 10, 1);

// הפעולה isBefore מחזירה אמת אם התאריך הראשון קודם מהתאריך השני
System.out.println(firstDate.isBefore(secondDate)); // Output: true

// הפעולה isAfter מחזירה אמת אם התאריך הראשון אחרי התאריך השני
System.out.println(firstDate.isAfter(secondDate)); // Output: false
```

ניתן גם לחשב את הפרש הזמן בין שתי תאריכים באמצעות הפעולה `between` מתוך החבילה `ChronoUnit`.

```java
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

LocalDate startDate = LocalDate.of(2021, 9, 1);
LocalDate endDate = LocalDate.of(2021, 10, 1);

// הפעולה between מחזירה את הפרש הימים בין שני התאריכים
long daysBetween = ChronoUnit.DAYS.between(startDate, endDate);
System.out.println("There are " + daysBetween + " days between " + startDate + " and " + endDate);
// Output: "There are 30 days between 2021-09-01 and 2021-10-01"
```

כמו כן, ניתן להשתמש בפעולה `compareTo` כדי להשוות בין שתי תאריכים ולקבל תוצאה שלילית, אפס או חיובית.

```java
import java.time.LocalDate;
import java.time.temporal.Chron