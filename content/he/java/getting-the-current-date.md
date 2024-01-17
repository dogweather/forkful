---
title:                "קבלת תאריך נוכחי"
html_title:           "Java: קבלת תאריך נוכחי"
simple_title:         "קבלת תאריך נוכחי"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?

קבלת התאריך הנוכחי היא פעולה חשובה בתכנות ב-Java. זה מאפשר למפתחים לצרף זמנים ותאריכים בתוך התוכנית שלהם, כמו לדוגמה לשמור תאריך של ריצת התוכנית או להפעיל משימות בזמנים מסוימים.

## איך לעשות זאת?

תהליך רכישת התאריך הנוכחי ב-Java פשוט וקצר. כאן אנו משתמשים בפקודה "LocalDate.now()" כדי לקבל את התאריך הנוכחי כפי שמוצג למטה:

```Java
LocalDate today = LocalDate.now();
System.out.println(today);
```

פלט:
"2020-08-16"

ניתן גם לקבל את התאריך הנוכחי בפורמט שונה באמצעות פקודות נוספות כמו למשל "DateTimeFormatter". הנה דוגמה להדפסת התאריך בפורמט שאנו בוחר:

```Java
LocalDate today = LocalDate.now();
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
String formattedDate = today.format(formatter);
System.out.println(formattedDate);
```

פלט:
"16/08/2020"

## חקירה מעמיקה

קבלת התאריך הנוכחי ב-Java מאוד חשובה כי היא תורמת לפיתוח תוכניות יעילים יותר ומאפשרת גם תיעוד מדויק של זמנים בתוך התוכנית. בנוסף לקבלת התאריך הנוכחי, ניתן גם לעשות שימוש בפקודות נוספות כגון "LocalDateTime" כדי לקבל תאריך עם זמן מדויק.

כארציה: לקבל תאריך עם שעה נוכחית ולהציג אותו בפורמט של שעה כדי למנוע בלבול:

```Java
LocalDateTime now = LocalDateTime.now();
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("hh:mm a");
String formattedTime = now.format(formatter);
System.out.println(formattedTime);
```

פלט:
"11:25 AM"

## ראה גם

למידע נוסף על פקודות ומתודות נוספות בספריית התאריך ב-Java, ניתן לבקר בקישור הבא:

[https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)

ולפרטים נוספים על קבלת התאריך הנוכחי בפיתוח Java, ניתן לקרוא את המאמר הזה:

[https://www.baeldung.com/java-get-current-date-time](https://www.baeldung.com/java-get-current-date-time)