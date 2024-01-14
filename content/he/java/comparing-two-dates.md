---
title:                "Java: השוואת שתי תאריכים"
simple_title:         "השוואת שתי תאריכים"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

##למה

למה מאמתים במיוחד תאריכים מול אחד אחר ומבינים את הקשר ביניהם?

כאשר אנחנו עובדים עם תאריכים בתכנות יש לנו לפעמים את הצורך להשוות תאריכים שונים זה לכוון זה. זה יכול להיות מועיל במצבים שונים, למשל כאשר נרצה לבדוק אם תאריך מסוים נמצא בתחום זמן מסוים או לצורך סידור תאריכים בסדר נכון. השוואת תאריכים יכולה להיות שימושית גם כאשר אנחנו תוכננו לבנות מסוים החלטות מבוססות על צורת התאריך.

##איך לעשות זאת

בתחילת הקוד שלך יש לייבא את חבילת התאריכים של יבעות ומדול תוך שמירה על הערך של התאריך הנוכחי. לאחר מכן נשתמש בפעולות עזר מצויינות בתאריך כדי להשוות בין התאריכים.

```Java
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

// שמירה על התאריך הנוכחי
LocalDate currentDate = LocalDate.now();

// השוואת תאריך נתון אל תאריך נוכחי עם הפעולה of()
LocalDate comparisonDate = LocalDate.of(2020, 12, 31);

// הליך השוואה עם הפעולה until()
long daysBetween = ChronoUnit.DAYS.between(currentDate, comparisonDate);

System.out.println("מספר הימים בין " + currentDate + " עד " + comparisonDate + " הוא: " + daysBetween + " ימים.");
```

הפלט של הקוד הנ"ל יהיה:

```
מספר הימים בין 2020-02-08 עד 2020-12-31 הוא: 327 ימים.
```

לדוגמה, אם נרצה לבדוק אם תאריך נתון נמצא בתחום זמן מסוים, אנחנו יכולים להשתמש בפעולת isAfter() או isBefore() או על ידי השוואה ישירה של התאריך הנתון עם תאריך נוכחי.

##נכנסים לעומק

בדרך כלל, כאשר אנו משווים בין שני תאריכים