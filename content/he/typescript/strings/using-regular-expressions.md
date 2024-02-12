---
title:                "שימוש בביטויים רגולריים"
aliases: - /he/typescript/using-regular-expressions.md
date:                  2024-02-03T19:19:38.732715-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש בביטויים רגולריים"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
ביטויים סדירים, או regex, הם כלי חזק להתאמת דפוסים וחיפוש בתכנות. תכנתים משתמשים ב-regex למשימות כמו אימות קלט מהמשתמש, חיפוש בטקסט, או מניפולציות במחרוזות כי הוא יעיל וגמיש.

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
