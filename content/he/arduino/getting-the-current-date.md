---
title:                "לקבל את התאריך הנוכחי"
html_title:           "Arduino: לקבל את התאריך הנוכחי"
simple_title:         "לקבל את התאריך הנוכחי"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה

אנחנו חיים בעולם ממוגָּבֶה בטכנולוגיות חדשות ומתקדמות מדי יום. אחד החדשות הכי מרתקים היא איזה דברים אנחנו יכולים לעשות עם קוד. מכאן מגיע לנו שאלת למה אנחנו רוצים לדעת כיצד להפעיל את הקוד שלנו ולקבל את התוצאות הרצויות. במאמר הזה, אנחנו נדבר על כיצד לכתוב קוד בעזרת הארדווינו הנוכחי לקבלת התאריך הנכון.

## איך לעשות זאת

הארדווינו מספק מגוון רחב של ספריות ופונקציות לשימוש. אחת מהן היא הפונקציה "millis()", שמחזירה את הזמן הנוכחי במילישניות (כלומר, אלפיות שנייה). באמצעות פונקציה זו, אנחנו יכולים לחשב את כמות המילישניות שעברו מתאריך מסוים, ומכאן לחשב את התאריך הנוכחי. לדוגמה:

```
Arduino millis() קוד: 

unsigned long currentTime = millis();
unsigned long pastTime = 1613038800000;

unsigned long secondsDiff = (currentTime - pastTime)/1000;
unsigned long minutesDiff = secondsDiff/60;
unsigned long hoursDiff = (minutesDiff/60)%24;

// חישוב התאריך הנוכחי מתוך הזמן החלף
unsigned long currentDay = (hoursDiff/24)%31 + 1; // חשיבה כי יש 31 ימים בחודש

// כאן ניתן לחשב גם את החודש ושנת התאריך הנוכחי לפי הצורך


// תצוגת התאריך הנוכחי במסך הסיריאל מוניטור
Serial.print("התאריך הנוכחי הוא: ");
Serial.print(currentDay);
Serial.print(" במחזור החודש");

```

פלט צפוי:

```
התאריך הנוכחי הוא: 10 במחזור החודש
```

## לכנות לעומק

כעת שאנחנו כבר ראינו איך לחשב את התאריך הנכון בעזרת הפונקציה "millis()", חשוב להבין שבאמצעות טכנולוגיות חדשות כמו ארד