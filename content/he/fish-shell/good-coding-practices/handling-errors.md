---
date: 2024-01-26 00:52:12.241600-07:00
description: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  \ \u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05E1\u05E7\u05E8\u05D9\u05E4\u05D8 \u05E9\
  \u05DC\u05DA \u05DC\u05D4\u05EA\u05DE\u05D5\u05D3\u05D3 \u05E2\u05DD \u05D4\u05DC\
  \u05D0 \u05E6\u05E4\u05D5\u05D9 \u05D1\u05D7\u05DF. \u05D0\u05E0\u05D5 \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E0\u05D4\u05DC\
  \ \u05DB\u05E9\u05DC \u05D1\u05DC\u05D9 \u05DC\u05D4\u05E4\u05D5\u05DA \u05D0\u05EA\
  \ \u05E9\u05D9\u05E2\u05E8\u05DD \u05E9\u05DC \u05D4\u05DE\u05E9\u05EA\u05DE\u05E9\
  \u05D9\u05DD \u05E9\u05DC\u05E0\u05D5 \u05DC\u05D0\u05E4\u05D5\u05E8."
lastmod: '2024-03-11T00:14:13.560093-06:00'
model: gpt-4-1106-preview
summary: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  \ \u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05E1\u05E7\u05E8\u05D9\u05E4\u05D8 \u05E9\
  \u05DC\u05DA \u05DC\u05D4\u05EA\u05DE\u05D5\u05D3\u05D3 \u05E2\u05DD \u05D4\u05DC\
  \u05D0 \u05E6\u05E4\u05D5\u05D9 \u05D1\u05D7\u05DF. \u05D0\u05E0\u05D5 \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E0\u05D4\u05DC\
  \ \u05DB\u05E9\u05DC \u05D1\u05DC\u05D9 \u05DC\u05D4\u05E4\u05D5\u05DA \u05D0\u05EA\
  \ \u05E9\u05D9\u05E2\u05E8\u05DD \u05E9\u05DC \u05D4\u05DE\u05E9\u05EA\u05DE\u05E9\
  \u05D9\u05DD \u05E9\u05DC\u05E0\u05D5 \u05DC\u05D0\u05E4\u05D5\u05E8."
title: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
טיפול בשגיאות מאפשר לסקריפט שלך להתמודד עם הלא צפוי בחן. אנו עושים זאת כדי לנהל כשל בלי להפוך את שיערם של המשתמשים שלנו לאפור.

## איך לעשות זאת:
כדי לתפוס שגיאות ב-Fish, נשען על הפקודה `status` ועל תנאיים. נניח ש־`ping` נכשל; הנה איך לזהות זאת:

```fish
ping -c 1 example.com
if not status is-success
    echo "משהו דגיג קרה עם הפינג."
end
```

פלט לדוגמא אם `ping` נכשל:

```
משהו דגיג קרה עם הפינג.
```

כדי לטפל בקוד שגיאה מסוים, השתמש ב־`status --is`:

```fish
false
if status --is 1
    echo "תפסת שגיאה עם קוד 1."
end
```

פלט לדוגמא:
```
תפסת שגיאה עם קוד 1.
```

לגישה יותר מוצקה, שקול להשתמש בפונקציה:

```fish
function try_ping
    ping -c 1 example.com
    or begin
        echo "הפינג נכשל עם מצב $status"
        return 1
    end
end

try_ping
```

## עיון מעמיק
טיפול בשגיאות ב-Fish אינו תואם לפרדיגמת `try/catch` שאתה עשוי להכיר משפות עיליות יותר. במקום זאת, יש לך סטטוסים יציאה ישירים המסופקים על ידי הפקודה `status`.

מבחינה היסטורית, במערכות דמויות Unix, סטטוס יציאה של `0` מציין הצלחה, בעוד כל ערך שאינו אפס מציין שגיאה, שלרוב משקף סיבות כישלון שונות. קונבנציה זו משמשת רוב כלי השורה הפקודה ולכן גם על ידי Fish עצמו.

חלופות לבדיקות `status` ב-Fish כוללות טיפול באותות באמצעות `trap` בשללים אחרים, אך Fish מעדיף בדיקות מצב יותר מפורשות, מכיוון שהן נקיות יותר ופחות נוטות לתופעות לוואי.

מבחינה טכנית, טיפול בשגיאות ב-Fish נשאר פשוט ועדיין עוצמתי, במידה רבה בזכות האופי הלא חוסם שלו ועל הדגש על תחביר ברור, כמו שמוצג בדוגמאות. קודי השגיאות משתלבים יפה עם פונקציות, מה שמאפשר ניהול שגיאות מודולרי וקריא.

## ראה גם
- תיעוד Fish על תנאים: https://fishshell.com/docs/current/language.html#conditionals
- הדרכת Fish על טיפול בשגיאות: https://fishshell.com/docs/current/tutorial.html#error-handling
