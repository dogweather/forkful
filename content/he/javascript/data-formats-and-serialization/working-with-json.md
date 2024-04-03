---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:36.814193-07:00
description: "JSON (JavaScript Object Notation) \u05D4\u05D5\u05D0 \u05E4\u05D5\u05E8\
  \u05DE\u05D8 \u05E7\u05DC \u05DE\u05E9\u05E7\u05DC \u05DC\u05D4\u05D7\u05DC\u05E4\
  \u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05E7\u05DC \u05DC\u05E7\u05E8\u05D9\
  \u05D0\u05D4 \u05D5\u05DB\u05EA\u05D9\u05D1\u05D4 \u05E2\u05DC \u05D9\u05D3\u05D9\
  \ \u05D1\u05E0\u05D9 \u05D0\u05D3\u05DD \u05D5\u05DC\u05E0\u05D9\u05EA\u05D5\u05D7\
  \ \u05D5\u05D9\u05E6\u05D9\u05E8\u05D4 \u05E2\u05DC \u05D9\u05D3\u05D9 \u05DE\u05DB\
  \u05D5\u05E0\u05D5\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\
  \u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D5 \u05DC\u05D0\u05D7\u05E1\u05D5\u05DF\
  \u2026"
lastmod: '2024-03-13T22:44:40.010314-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) \u05D4\u05D5\u05D0 \u05E4\u05D5\u05E8\u05DE\
  \u05D8 \u05E7\u05DC \u05DE\u05E9\u05E7\u05DC \u05DC\u05D4\u05D7\u05DC\u05E4\u05EA\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05E7\u05DC \u05DC\u05E7\u05E8\u05D9\u05D0\
  \u05D4 \u05D5\u05DB\u05EA\u05D9\u05D1\u05D4 \u05E2\u05DC \u05D9\u05D3\u05D9 \u05D1\
  \u05E0\u05D9 \u05D0\u05D3\u05DD \u05D5\u05DC\u05E0\u05D9\u05EA\u05D5\u05D7 \u05D5\
  \u05D9\u05E6\u05D9\u05E8\u05D4 \u05E2\u05DC \u05D9\u05D3\u05D9 \u05DE\u05DB\u05D5\
  \u05E0\u05D5\u05EA."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON"
weight: 38
---

## מה ולמה?

JSON (JavaScript Object Notation) הוא פורמט קל משקל להחלפת נתונים, קל לקריאה וכתיבה על ידי בני אדם ולניתוח ויצירה על ידי מכונות. מתכנתים משתמשים בו לאחסון והעברת נתונים ביישומי אינטרנט, מה שהופך אותו לעמוד התווך של תקשורת API ושירותי ווב מודרניים.

## איך לעשות:

### ניתוח JSON
כדי להמיר מחרוזת JSON לאובייקט של JavaScript, השתמשו ב-`JSON.parse()`.

```javascript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
const obj = JSON.parse(jsonString);
console.log(obj.name); // פלט: John
```

### הפיכת אובייקטים של JavaScript למחרוזות JSON
כדי להמיר אובייקט של JavaScript חזרה למחרוזת JSON, השתמשו ב-`JSON.stringify()`.

```javascript
const user = { name: "Jane", age: 25, city: "London" };
const jsonString = JSON.stringify(user);
console.log(jsonString); // פלט: {"name":"Jane","age":25,"city":"London"}
```

### עבודה עם קבצים ב-Node.js
כדי לקרוא קובץ JSON ולהמיר אותו לאובייקט בסביבת Node.js, תוכלו להשתמש במודול `fs`. הדוגמא הזו מניחה שיש לכם קובץ בשם `data.json`.

```javascript
const fs = require('fs');

fs.readFile('data.json', 'utf-8', (err, data) => {
    if (err) throw err;
    const obj = JSON.parse(data);
    console.log(obj);
});
```

לכתיבת אובייקט לקובץ JSON:

```javascript
const fs = require('fs');
const user = { name: "Mike", age: 22, city: "Berlin" };

fs.writeFile('user.json', JSON.stringify(user, null, 2), (err) => {
    if (err) throw err;
    console.log('Data written to file');
});
```

### ספריות צד שלישי
עבור פעולות JSON מורכבות, מסגרות וספריות כמו `lodash` יכולות לפשט משימות, אך לפעולות בסיסיות, לעיתים קרובות פונקציות ה-JavaScript הנטיביות מספיקות. עבור יישומים בקנה מידה גדול או קריטיים מבחינת ביצועים, תוכלו לשקול ספריות כמו `fast-json-stringify` להמרת JSON מהירה יותר או `json5` לניתוח והמרה באמצעות פורמט JSON גמיש יותר.

ניתוח עם `json5`:
```javascript
const JSON5 = require('json5');

const jsonString = '{name:"John", age:30, city:"New York"}';
const obj = JSON5.parse(jsonString);
console.log(obj.name); // פלט: John
```

דוגמאות אלו כוללות פעולות בסיסיות עם JSON ב-JavaScript, מושלמות למתחילים המעברים משפות אחרות ומחפשים לטפל בנתונים ביישומי אינטרנט ביעילות.
