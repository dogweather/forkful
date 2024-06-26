---
date: 2024-01-20 17:31:48.356745-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D7\u05D9\u05E9\
  \u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D1-JS \u05D4\u05D9\u05D4\
  \ \u05EA\u05DE\u05D9\u05D3 \u05DE\u05D5\u05E8\u05DB\u05D1 \u05E2\u05E7\u05D1 \u05DE\
  \u05D5\u05E8\u05DB\u05D1\u05D5\u05EA \u05E9\u05E2\u05D5\u05DF \u05D4\u05E2\u05D5\
  \u05DC\u05DD. \u05D4\u05EA\u05DE\u05D9\u05DB\u05D4 \u05D1\u05D0\u05D6\u05D5\u05E8\
  \u05D9 \u05D6\u05DE\u05DF \u05D5\u05E9\u05D9\u05E0\u05D5\u05D9\u05D9\u05DD \u05D0\
  \u05D5\u05D8\u05D5\u05DE\u05D8\u05D9\u05D9\u05DD \u05DC-UTC \u05D9\u05DB\u05D5\u05DC\
  \u05D4 \u05DC\u05E7\u05E8\u05D5\u05EA \u05D0\u05D5\u05D8\u05D5\u05DE\u05D8\u05D9\
  \u05EA, \u05D0\u05D1\u05DC \u05DC\u05E4\u05E2\u05DE\u05D9\u05DD \u05D6\u05D4 \u05E2\
  \u05DC\u05D5\u05DC\u2026"
lastmod: '2024-04-05T22:50:54.055697-06:00'
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD\
  \ \u05D1-JS \u05D4\u05D9\u05D4 \u05EA\u05DE\u05D9\u05D3 \u05DE\u05D5\u05E8\u05DB\
  \u05D1 \u05E2\u05E7\u05D1 \u05DE\u05D5\u05E8\u05DB\u05D1\u05D5\u05EA \u05E9\u05E2\
  \u05D5\u05DF \u05D4\u05E2\u05D5\u05DC\u05DD."
title: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\
  \u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8"
weight: 26
---

## איך לעשות:
```Javascript
// חישוב תאריך עתידי - עוד 10 ימים מהיום
let today = new Date();
let futureDate = new Date(today);
futureDate.setDate(today.getDate() + 10);
console.log(futureDate);

// חישוב תאריך בעבר - 5 ימים לפני היום
let pastDate = new Date(today);
pastDate.setDate(today.getDate() - 5);
console.log(pastDate);
```
פלט לדוגמא:
```
2023-04-12T12:00:00.000Z (התאריך העתידי)
2023-03-28T12:00:00.000Z (התאריך בעבר)
```

## צלילה עמוקה:
חישוב תאריכים ב-JS היה תמיד מורכב עקב מורכבות שעון העולם. התמיכה באזורי זמן ושינויים אוטומטיים ל-UTC יכולה לקרות אוטומטית, אבל לפעמים זה עלול ליצור בעיות. אלטרנטיבות כוללות ספריות כמו Moment.js ו-date-fns, אשר מציעות חווית משתמש נוחה יותר ופונקציונאליות רחבה יותר. קוד לוקאלי צריך לתמוך בתאריכים עבריים וטיפול באירועים היסטוריים ודתיים.

## ראה גם:
- [MDN - Date object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [date-fns Documentation](https://date-fns.org/)
- [Moment.js Documentation](https://momentjs.com/docs/#/use-it/)
- [Hebrew calendar programming](https://www.hebcal.com/home/developer-apis)
