---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:47.651634-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD CSV (\u05E2\u05E8\u05DB\u05D9\
  \u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\u05DD \u05D1\u05E4\u05E1\u05D9\u05E7\
  \u05D9\u05DD) \u05D1-JavaScript \u05DB\u05D5\u05DC\u05DC\u05EA \u05E4\u05E8\u05E1\
  \u05D5\u05E8 \u05D0\u05D5 \u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D1\u05E6\u05D9\
  \ CSV \u05D1\u05DB\u05D3\u05D9 \u05DC\u05D9\u05D9\u05D1\u05D0 \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05D8\u05D1\u05DC\u05D0\u05D9\u05D9\u05DD \u05DE\u05DE\u05E7\
  \u05D5\u05E8\u05D5\u05EA \u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05D9\u05DD \u05D0\
  \u05D5 \u05DC\u05D9\u05D9\u05E6\u05D0 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DC\
  \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05EA\u05D5\u05DB\u05E0\u05D9\u05D5\u05EA\u2026"
lastmod: 2024-02-19 22:04:59.285561
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD CSV (\u05E2\u05E8\u05DB\u05D9\
  \u05DD \u05DE\u05D5\u05E4\u05E8\u05D3\u05D9\u05DD \u05D1\u05E4\u05E1\u05D9\u05E7\
  \u05D9\u05DD) \u05D1-JavaScript \u05DB\u05D5\u05DC\u05DC\u05EA \u05E4\u05E8\u05E1\
  \u05D5\u05E8 \u05D0\u05D5 \u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D1\u05E6\u05D9\
  \ CSV \u05D1\u05DB\u05D3\u05D9 \u05DC\u05D9\u05D9\u05D1\u05D0 \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05D8\u05D1\u05DC\u05D0\u05D9\u05D9\u05DD \u05DE\u05DE\u05E7\
  \u05D5\u05E8\u05D5\u05EA \u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05D9\u05DD \u05D0\
  \u05D5 \u05DC\u05D9\u05D9\u05E6\u05D0 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DC\
  \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05EA\u05D5\u05DB\u05E0\u05D9\u05D5\u05EA\u2026"
title: "\u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD CSV"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם CSV (ערכים מופרדים בפסיקים) ב-JavaScript כוללת פרסור או יצירת קבצי CSV בכדי לייבא נתונים טבלאיים ממקורות חיצוניים או לייצא נתונים לשימוש בתוכניות אחרות. מתכנתים עושים זאת משום שזה מאפשר החלפת נתונים קלה וקלילה בין יישומים, מסדי נתונים ומערכות, שם פורמטים מורכבים יותר כמו JSON עשויים להיות מוגזמים.

## איך לעשות:

ב-JavaScript אין פונקציונליות מובנית לפרסור או להמרת נתונים ל-CSV כמו שיש עם JSON. עם זאת, ניתן לנהל נתוני CSV בקלות על ידי שימוש ב-JavaScript גולמי למטלות פשוטות יותר או בעזרת ספריות חזקות כמו `PapaParse` לתסריטים מורכבים יותר.

### פרסור בסיסי עם JavaScript גולמי
לפרסר מחרוזת CSV פשוטה למערך של אובייקטים:

```javascript
const csv = `name,age,city
John,23,New York
Jane,28,Los Angeles`;

function parseCSV(csv) {
  const lines = csv.split("\n");
  const result = [];
  const headers = lines[0].split(",");

  for (let i = 1; i < lines.length; i++) {
    const obj = {};
    const currentline = lines[i].split(",");
    
    for (let j = 0; j < headers.length; j++) {
      obj[headers[j]] = currentline[j];
    }
    result.push(obj);
  }
  
  return result;
}

console.log(parseCSV(csv));
```
פלט:

```
[
  { name: 'John', age: '23', city: 'New York' },
  { name: 'Jane', age: '28', city: 'Los Angeles' }
]
```

### יצירת CSV בסיסית עם JavaScript גולמי
להמיר מערך של אובייקטים למחרוזת CSV:

```javascript
const data = [
  { name: 'John', age: 23, city: 'New York' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

function arrayToCSV(arr) {
  const csv = arr.map(row => 
    Object.values(row).join(',')
  ).join('\n');
  
  return csv;
}

console.log(arrayToCSV(data));
```

פלט:

```
John,23,New York
Jane,28,Los Angeles
```

### שימוש ב-PapaParse למשימות CSV מורכבות
לתסריטים מורכבים יותר, `PapaParse` היא ספריה חזקה המתאימה לפרסור ויצירת קבצי CSV עם אפשרויות לזרמים, עובדים, וטיפול בקבצים גדולים.

פרסור קובץ או מחרוזת CSV עם PapaParse:

```javascript
// לאחר הוספת PapaParse לפרויקט שלך
const Papa = require('papaparse');
const csv = `name,age,city
John,23,New York
Jane,28,Los Angeles`;

Papa.parse(csv, {
  complete: function(results) {
    console.log("Parsed:", results.data);
  }
});
```

מייצר:

```
Parsed: [
  ["name", "age", "city"],
  ["John", "23", "New York"],
  ["Jane", "28", "Los Angeles"]
]
```

המרת מערך למחרוזת CSV עם PapaParse:

```javascript
const data = [
  { name: 'John', age: 23, city: 'New York' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

console.log(Papa.unparse(data));
```

מייצר:

```
name,age,city
John,23,New York
Jane,28,Los Angeles
```

הדוגמאות הללו ממחישות טיפול בסיסי ומתקדם ב-CSV ב-JavaScript, ומאפשרות החלפת נתונים קלה ביישומי ווב ומעבר להם.
