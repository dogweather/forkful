---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:38.732715-07:00
description: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E1\u05D3\u05D9\u05E8\u05D9\
  \u05DD, \u05D0\u05D5 regex, \u05D4\u05DD \u05DB\u05DC\u05D9 \u05D7\u05D6\u05E7 \u05DC\
  \u05D4\u05EA\u05D0\u05DE\u05EA \u05D3\u05E4\u05D5\u05E1\u05D9\u05DD \u05D5\u05D7\
  \u05D9\u05E4\u05D5\u05E9 \u05D1\u05EA\u05DB\u05E0\u05D5\u05EA. \u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1-regex \u05DC\
  \u05DE\u05E9\u05D9\u05DE\u05D5\u05EA \u05DB\u05DE\u05D5 \u05D0\u05D9\u05DE\u05D5\
  \u05EA \u05E7\u05DC\u05D8 \u05DE\u05D4\u05DE\u05E9\u05EA\u05DE\u05E9, \u05D7\u05D9\
  \u05E4\u05D5\u05E9 \u05D1\u05D8\u05E7\u05E1\u05D8, \u05D0\u05D5 \u05DE\u05E0\u05D9\
  \u05E4\u05D5\u05DC\u05E6\u05D9\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:38.900929-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E1\u05D3\u05D9\u05E8\u05D9\
  \u05DD, \u05D0\u05D5 regex, \u05D4\u05DD \u05DB\u05DC\u05D9 \u05D7\u05D6\u05E7 \u05DC\
  \u05D4\u05EA\u05D0\u05DE\u05EA \u05D3\u05E4\u05D5\u05E1\u05D9\u05DD \u05D5\u05D7\
  \u05D9\u05E4\u05D5\u05E9 \u05D1\u05EA\u05DB\u05E0\u05D5\u05EA."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD"
weight: 11
---

## איך לעשות:
בואו נקפוץ ל-TypeScript ונראה איך regex משמש למשימות נפוצות.

```TypeScript
// הגדרת דפוס regex לכתובת דוא"ל
const emailPattern = /\S+@\S+\.\S+/;

// בדיקה אם מחרוזת תואמת לדפוס הדוא"ל
const email = "user@example.com";
console.log(emailPattern.test(email)); // פלט: true

// חיפוש והחלפת ספרות במחרוזת
const replaceDigits = "Item 25 costs $30".replace(/\d+/g, '#');
console.log(replaceDigits); // פלט: "Item # costs $#"

// חילוץ חלקים ספציפיים ממחרוזת באמצעות קבוצות לכידה
const data = "April 10, 2021";
const datePattern = /(\w+) (\d+), (\d+)/;
const [, month, day, year] = datePattern.exec(data) || [];
console.log(month, day, year); // פלט: "April" "10" "2021"
```

## ניתוח עמוק
בשנות ה-50, המתמטיקאי סטפן קלין הציג ביטויים סדירים כדגם לייצוג שפות סדירות, שלאחר מכן הפכו לחיוניות במדעי המחשב. מאוחר יותר, regex הפך לנפוץ בתכנות לטיפול בטקסט.

למרות ש-regex הוא כלי כמו סכין שווייצרית לפעולות עם מחרוזות, הוא לא בלי אלטרנטיבות. בהתאם למורכבות של המשימה, לפעמים שיטות מחרוזת כמו `includes()`, `startsWith()`, `endsWith()`, או אפילו ניתוח עם ספרייה יכולות להיות טובות יותר. לדוגמה, ניתוח מחרוזת JSON מורכבת באמצעות regex יכול להיות סיוט—השתמשו במנתח JSON במקום.

בנוגע ליישום, regex ב-JavaScript ו-TypeScript מבוסס על המפרט של שפת ECMAScript. מאחורי הקלעים, מנועים משתמשים במכונות מצבים להתאמת דפוסים ביעילות. שווה לציין שפעולות regex יכולות להיות יקרות מבחינת ביצועים, במיוחד עם דפוסים שנכתבו באופן גרוע—היזהרו מ-"גביע החוזר הקטסטרופלי".

## ראו גם
- MDN Web Docs על ביטויים סדירים: [MDN Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- Regex101: כלי לבדוק ולפתור בעיות בדפוסי regex [Regex101](https://regex101.com/)
- הספר "שליטה בביטויים סדירים" להבנה עמוקה: [O'Reilly](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)
