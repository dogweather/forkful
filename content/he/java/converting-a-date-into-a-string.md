---
title:                "המרת תאריך למחרוזת"
html_title:           "Java: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

מה ולמה? 
המרת תאריך למחרוזת היא פעולת תכנות המקבלת תאריך מסוים וממירה אותו למחרוזת בפורמט מסוים. תהליך זה נחוץ לצורך הצגת תאריך עבור משתמשים וליישומי ממשק משתמש. 

כיצד לעשות: 
שני דוגמאות קוד ותוצאות דוגמא יתקלו בתוך בלוק קוד ```Java ... ```. 
- דוגמא 1: 
```
// Imports ייבואים 
import java.time.LocalDate; 
import java.time.format.DateTimeFormatter;

// תאריך קבוע 
LocalDate date = LocalDate.of(2021, 11, 5); 

// המרת תאריך למחרוזת בפורמט מבוקש 
String stringDate = date.format(DateTimeFormatter.ofPattern("dd/MM/yyyy")); 

// הדפסת התוצאה 
System.out.println(stringDate); 
// פלט צפוי: 05/11/2021
```
- דוגמא 2: 
```
// Imports ייבואים 
import java.util.Calendar; 
import java.text.SimpleDateFormat;

// תאריך מאתחל 
Calendar date = Calendar.getInstance(); 
date.set(2021, 3, 26);

// המרת תאריך למחרוזת בפורמט מבוקש 
SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy"); 
String stringDate = sdf.format(date.getTime()); 

// הדפסת התוצאה 
System.out.println(stringDate); 
// פלט צפוי: 26/04/2021
```

צלילה עמוקה: 
ההמרה של תאריך למחרוזת היא פעולה חשובה בתכנות המאפשרת למנוע באגים ולהציג מידע למשתמשים בפורמט נוח וקריא. כמו כן, קיימות גם פתרונות אחרים להמרת תאריך כגון: להשתמש בכלי ניהול תאריכים מוכנים כמו Joda-Time או ליצור ממשק עצמאי שלנו. 
בכניסת Java 8, נוספו מספר דפוסי תאריך חדשים למחלופים של דוגמאות המסופקות בכתב קוד שנמצא בחבילה ```java.time.*```. יישום קיים של הוראות ISO-8601 deals מאפשר להמיר תאריך למחרוזת וכן להתאים אותו לפונקציות שונות לדוגמא: השוואת תאריכים, הוספת וחיסור שני תאריכים ועוד. הדור הירושלמי של תקני תאריך שמאפשרת הגדרת תאריך מוכלל כמו Joda-Time ```java.util.Calendar```.

ראו גם: 
למידע נוסף על ההמרה של תאריך למחרוזת והחישובים השונים הקשורים לזה, ניתן לעיין במסמכי ייעוץ ומאמרי תופעה אחרים לגבי ניהול תאריכים בשפת Java. לדוגמא: [מסמכי זמן ותאריך בשפת Java](https://docs.oracle.com/javase/tutorial/datetime/index.html), [מדריך תאריכים ושעות ב-Javatpoint](https://www.javatpoint.com/java-date-time).