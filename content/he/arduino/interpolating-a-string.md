---
title:                "אינטרפולציה של מחרוזת"
html_title:           "Arduino: אינטרפולציה של מחרוזת"
simple_title:         "אינטרפולציה של מחרוזת"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
אינטרפולציה של מחרוזת היא ההכנסה של משתנים לתוך מחרוזת. זה מאפשר למתכנתים ליצור מחרוזות מעורבות באופן יעיל ונוח יותר.

## איך לעשות?
חתיכת קוד שמדגימה אינטרפולציה של מחרוזת ב-Arduino:

```Arduino
char temp[100];
int sensorVal = analogRead(A0);
sprintf(temp, "ערך החיישן הוא %d", sensorVal);
Serial.println(temp);
```
הפלט הצפוי הוא: "ערך החיישן הוא x" כאשר "x" הוא הערך שהוחזר מהקריאה ל-analogRead(A0).

## צלילה עמוקה
אינטרפולציה של מחרוזות היא שיטה שלה השפעות עמוקות בתחום התכנות מאז שנולדה.  על פי מקורות, האינטרפולציה של מחרוזות התפתחה מתחילה בשפת התכנות Perl. בשפות תכנות אחרות, כמו Python וJavaScript, ישנם גם תחליפים לsprintf. עם זאת, בארדואינו, הפונקציה sprintf מהווה את הגישה הנפוצה ביותר לאינטרפולציה של מחרוזות.

## ראה גם
עיינו במקורות הבאים למידע נוסף:
- [היסטוריה של אינטרפולציה של מחרוזות](https://stackoverflow.com/questions/89007/string-interpolation-in-javascript)