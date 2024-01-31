---
title:                "עבודה עם קבצי CSV"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם קבצי CSV מאפשרת לנהל ולעבד נתונים בפורמט טבלאי: כל שורה היא רשומה, וכל עמודה מופרדת בפסיק. תכניתנים עושים זאת כדי לייבא, לעבד ולייצא נתונים בצורה נוחה וכללית.

## איך לעשות:
```javascript
// קריאת CSV
const fs = require('fs');
const csvData = fs.readFileSync('data.csv', 'utf8');

// פיצול הנתונים לשורות ועמודות
const rows = csvData.split('\n').map(row => row.split(','));

// הדפסת השורה הראשונה
console.log(rows[0]);

// כתיבת CSV
const newData = [
  ['name', 'age', 'city'],
  ['Alice', '22', 'Wonderland'],
  ['Bob', '30', 'Builderland']
];
const csvContent = newData.map(e => e.join(',')).join('\n');
fs.writeFileSync('newData.csv', csvContent);

// הפלט יהיה: name,age,city
```

## עיון מעמיק
ה-CSV (Comma-Separated Values - ערכים מופרדי פסיקים) קיים כבר עשרות שנים ונחשב לפשוט וגמיש. ישנם אלטרנטיבות כמו JSON או XML שמאפשרות מבני נתונים מורכבים יותר. בעבודה עם CSV צריך לזכור לטפל במיוחד בשורות ריקות, גרשיות ופסיקים בתוך טקסט.

## ראו גם
- [MDN Web Docs - Working with Text](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch#uploading_json_data)
- [CSV on Wikipedia](https://en.wikipedia.org/wiki/Comma-separated_values)
- [Node.js fs Module](https://nodejs.org/api/fs.html)
