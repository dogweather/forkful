---
title:    "Elm: מאחד מחרוזות"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## למה
כתיבת קוד בלתי-מכווננת יכולה להיות מאתגרת וקשה לקריאה. הצרכים של המפתחים שונים מאוד בעבורי, לכן אני רוצה לחלק איתכם כיצד אפשר להשתמש בפונקציות של Elm שתעזור לכם לארגן את הקוד וליצור ביצועים מהירים יותר.

## כיצד לעשות זאת
חיבור מחרוזות הוא תהליך שמאפשר ליצור מחרוזות חדשות על ידי חיבור מחרוזות שונות יחד. הנה כמה דוגמאות לקוד Elm שמדגים את כיצד לחבר מחרוזות כדי ליצור מחרוזות חדשות:

```Elm
-- יצירת מחרוזת חדשה על ידי חיבור של שתי מחרוזות
newString = "שלום" ++ " לכולם"
```

```Elm
-- חיבור מחרוזת משתנה עם מחרוזת קבועה
name = "אליס"
hello = "שלום" ++ name
```

```Elm
-- חיבור מחרוזת למשתנה מספרי
count = 5
text = "יש לי " ++ (toString count) ++ " תפוחים"
```

פלט:

```
שלום לכולם
שלום אליס
יש לי 5 תפוחים
```

## לחקור עומק

בנוסף לחיבור מחרוזות, פונקציות נוספות זמינות ב- Elm שיסייעו לכם לעשות רבים דברים לדוגמה: חילוץ תווים מחרוזות, חילוץ תווים מלמעלה או מאיזור נתון במחרוזת ועוד. אתם יכולים למצוא מידע נוסף על פונקציות אלו ועוד בקישורים המוצגים כאן למטה.

## ראה גם
- [שפת תיכנות Elm - מה משמעותה וכיצד היא עוזרת לכם לפתור בעיות](https://medium.com/hej-elm/%D7%A9%D7%A4%D7%AA-%D7%AA%D7%99%D7%9B%D7%A0%D7%95%D7%AA-elm-%D7%9E%D7%94-%D7%9E%D7%A9%D7%9E%D7%A2%D7%95%D7%AA%D7%94