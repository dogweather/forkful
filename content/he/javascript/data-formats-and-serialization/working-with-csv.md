---
title:                "עובדים עם CSV"
aliases:
- he/javascript/working-with-csv.md
date:                  2024-02-03T19:20:47.651634-07:00
model:                 gpt-4-0125-preview
simple_title:         "עובדים עם CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
