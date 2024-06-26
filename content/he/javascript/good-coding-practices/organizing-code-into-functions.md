---
date: 2024-01-26 01:12:03.163642-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05D0\u05D5\
  \u05E4\u05DF \u05D4\u05D9\u05E1\u05D8\u05D5\u05E8\u05D9, \u05E9\u05E4\u05D5\u05EA\
  \ \u05EA\u05DB\u05E0\u05D5\u05EA \u05D0\u05D9\u05DE\u05E4\u05E8\u05D8\u05D9\u05D1\
  \u05D9\u05D5\u05EA \u05DB\u05DE\u05D5 \u05D2\u05E8\u05E1\u05D0\u05D5\u05EA \u05E7\
  \u05D3\u05D5\u05DE\u05D5\u05EA \u05E9\u05DC BASIC \u05D0\u05D5 Assembly \u05D7\u05E1\
  \u05E8\u05D5\u05EA \u05D0\u05EA \u05D4\u05D0\u05D1\u05E1\u05D8\u05E8\u05E7\u05E6\
  \u05D9\u05D4 \u05E9\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05DE\u05E1\
  \u05E4\u05E7\u05D5\u05EA. \u05DC\u05D0\u05D5\u05E8\u05DA \u05D4\u05D6\u05DE\u05DF\
  , \u05D4\u05DE\u05D5\u05E9\u05D2 \u05E9\u05DC \u05E7\u05D5\u05D3\u2026"
lastmod: '2024-04-05T21:53:41.016833-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05D0\u05D5\u05E4\u05DF \u05D4\u05D9\u05E1\u05D8\u05D5\u05E8\u05D9\
  , \u05E9\u05E4\u05D5\u05EA \u05EA\u05DB\u05E0\u05D5\u05EA \u05D0\u05D9\u05DE\u05E4\
  \u05E8\u05D8\u05D9\u05D1\u05D9\u05D5\u05EA \u05DB\u05DE\u05D5 \u05D2\u05E8\u05E1\
  \u05D0\u05D5\u05EA \u05E7\u05D3\u05D5\u05DE\u05D5\u05EA \u05E9\u05DC BASIC \u05D0\
  \u05D5 Assembly \u05D7\u05E1\u05E8\u05D5\u05EA \u05D0\u05EA \u05D4\u05D0\u05D1\u05E1\
  \u05D8\u05E8\u05E7\u05E6\u05D9\u05D4 \u05E9\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\
  \u05EA \u05DE\u05E1\u05E4\u05E7\u05D5\u05EA."
title: "\u05E1\u05D9\u05D3\u05D5\u05E8 \u05E7\u05D5\u05D3 \u05DC\u05EA\u05D5\u05DA\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA"
weight: 18
---

## איך לעשות:
```javascript
// הגדרת פונקציה לחישוב שטח מלבן
function calculateArea(width, height) {
  return width * height;
}

// קריאה לפונקציה והדפסת התוצאה
let area = calculateArea(5, 3);
console.log(area); // פלט: 15
```

```javascript
// קבץ פונקציות קשורות יחד
function greet(name) {
  console.log(`שלום, ${name}!`);
}

function farewell(name) {
  console.log(`להתראות, ${name}!`);
}

greet('Alice'); // פלט: שלום, Alice!
farewell('Bob'); // פלט: להתראות, Bob!
```

## צלילה עמוקה
באופן היסטורי, שפות תכנות אימפרטיביות כמו גרסאות קדומות של BASIC או Assembly חסרות את האבסטרקציה שפונקציות מספקות. לאורך הזמן, המושג של קוד מודולרי בשפות כמו C הציג את הרעיון שפירוק הקוד ליחידות (פונקציות או פרוצדורות) מוביל לארגון טוב יותר ולוגיקה ברורה יותר.

ב-JavaScript, מעבר לפונקציות רגילות, יש לנו פונקציות חץ מאז ES6 (2015) שמספקות תחביר עילי יותר ומתאימות לפונקציות שאינן שיטות.

אלטרנטיבות ושיפורים לארגון קוד ב-JavaScript כוללים גישות מונחות אובייקטים באמצעות מחלקות, או פרדיגמות תכנות פונקציונלית שמטפלות בפונקציות כנתינות מדרגה ראשונה.

במימוש, פונקציות JavaScript תומכות בקלוז'רים (closures), מספקות דרך לשמור גישה לסביבת הפונקציה אחרי הרצה, שזה עוצמתי לאינקפסולציה ויצירת פונקציות פקטוריה, בין היתר.

## ראה גם
- MDN Web Docs על פונקציות: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Functions
- דפוסי עיצוב ב-JavaScript: https://addyosmani.com/resources/essentialjsdesignpatterns/book/
- קוד נקי ב-JavaScript: https://github.com/ryanmcdermott/clean-code-javascript
