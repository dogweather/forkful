---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:38.503959-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\
  \u05E1\u05D8 \u05D1-JavaScript \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\
  \u05D5\u05D1\u05D5\u05EA \u05E7\u05E9\u05D5\u05E8\u05D4 \u05DC\u05D9\u05E6\u05D9\
  \u05E8\u05D4 \u05D5\u05E9\u05DE\u05D9\u05E8\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8 \u05E4\u05E9\u05D5\u05D8 \u05D5\u05E7\
  \u05E8\u05D9\u05D0 \u05DC\u05E6\u05D5\u05E8\u05DB\u05D9 \u05E8\u05D9\u05E9\u05D5\
  \u05DD, \u05D9\u05D9\u05E6\u05D5\u05D0 \u05E7\u05DC\u05D8 \u05DE\u05E9\u05EA\u05DE\
  \u05E9, \u05D0\u05D5 \u05E6\u05D5\u05E8\u05DB\u05D9 \u05E7\u05D5\u05E0\u05E4\u05D9\
  \u05D2\u05D5\u05E8\u05E6\u05D9\u05D4. \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\
  \u05E0\u05DC\u05D9\u05D5\u05EA\u2026"
lastmod: '2024-02-25T18:49:38.247300-07:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8 \u05D1-JavaScript \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\
  \u05D1\u05D5\u05EA \u05E7\u05E9\u05D5\u05E8\u05D4 \u05DC\u05D9\u05E6\u05D9\u05E8\
  \u05D4 \u05D5\u05E9\u05DE\u05D9\u05E8\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05D1\u05E4\u05D5\u05E8\u05DE\u05D8 \u05E4\u05E9\u05D5\u05D8 \u05D5\u05E7\u05E8\
  \u05D9\u05D0 \u05DC\u05E6\u05D5\u05E8\u05DB\u05D9 \u05E8\u05D9\u05E9\u05D5\u05DD\
  , \u05D9\u05D9\u05E6\u05D5\u05D0 \u05E7\u05DC\u05D8 \u05DE\u05E9\u05EA\u05DE\u05E9\
  , \u05D0\u05D5 \u05E6\u05D5\u05E8\u05DB\u05D9 \u05E7\u05D5\u05E0\u05E4\u05D9\u05D2\
  \u05D5\u05E8\u05E6\u05D9\u05D4. \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\
  \u05DC\u05D9\u05D5\u05EA\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת קובץ טקסט ב-JavaScript לעיתים קרובות קשורה ליצירה ושמירת נתונים בפורמט פשוט וקריא לצורכי רישום, ייצוא קלט משתמש, או צורכי קונפיגורציה. פונקציונליות זו חיונית ליישומים הדורשים לשמר נתונים מעבר למשך חיי התהליך של היישום, ומספקת דרך לאחסן ולשחזר או לשתף מידע בהמשך.

## איך לעשות:
בסביבת Node.js, ניתן להשתמש במודול המובנה `fs` (מערכת קבצים) לכתיבת קבצי טקסט. הדוגמה הזו מדגימה כתיבת טקסט לקובץ באופן אסינכרוני:

```javascript
const fs = require('fs');

const data = 'Hello, World! This is text to be written into a file.';

fs.writeFile('example.txt', data, (err) => {
  if (err) {
    throw err;
  }
  console.log('File has been written.');
});
```

פלט לדוגמא:
```
File has been written.
```

לכתיבת קובץ באופן סינכרוני, השתמש ב-`writeFileSync`:
```javascript
try {
  fs.writeFileSync('example.txt', data);
  console.log('File has been written.');
} catch (error) {
  console.error('Error writing file:', error);
}
```

בדפדפנים עכשוויים, ה-API של גישה למערכת קבצים מציג את היכולת לקרוא ולכתוב קבצים. עם זאת, שימושו כפוף להרשאות משתמש. הנה איך ליצור ולכתוב לקובץ:

```javascript
if ('showSaveFilePicker' in window) {
  const handle = await window.showSaveFilePicker();
  const writable = await handle.createWritable();
  await writable.write('Hello, World! This is browser text file writing.');
  await writable.close();
}
```

עבור תרחישים מורכבים יותר או כאשר עובדים עם קבצים גדולים, ייתכן שתעדיפו להשתמש בספריות צד שלישי כמו `FileSaver.js` לדפדפנים:

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.2/FileSaver.min.js"></script>
<script>
  const blob = new Blob(["Hello, World! This is text from FileSaver.js."], {type: "text/plain;charset=utf-8"});
  saveAs(blob, "example.txt");
</script>
```

זכרו, כתיבת קבצים בצד הלקוח (בדפדפנים) מוגבלת בשל דאגות בטיחות, וכל פעולה הדורשת שמירה על דיסק המשתמש המקומי לרוב תדרוש את הסכמתם המפורשת.
