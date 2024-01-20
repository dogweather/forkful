---
title:                "כתיבה לקובץ טקסט"
html_title:           "Bash: כתיבה לקובץ טקסט"
simple_title:         "כתיבה לקובץ טקסט"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
כתיבה לקובץ טקסט היא פעולת יצירה או עדכון של קובץ עם מידע בצורת טקסט. תכניתנים עושים זאת לשמירת נתונים, הגדרות או לוגים.

## How to:
ב-JavaScript, כתיבה לקובץ טקסט נעשית בצד השרת (Node.js), עם המודול `fs`.

```javascript
const fs = require('fs');

// כתיבה סינכרונית
fs.writeFileSync('example.txt', 'שלום עולם!', 'utf8');

// כתיבה אסינכרונית
fs.writeFile('example.txt', 'שלום שוב!', 'utf8', (err) => {
  if (err) throw err;
  console.log('הקובץ נשמר!');
});
```

פלט (לכתיבה אסינכרונית):
```
הקובץ נשמר!
```

## Deep Dive
בעבר, כתיבה לקבצי טקסט הייתה מורכבת יותר ודרשה שימוש ב-APIs שונים בשפות שונות. ישנן חלופות למודול `fs`, כמו `fs-extra` המספק פונקציונאליות נוספת. כשכותבים לקובץ טקסט, חשוב לשקול את הפרמטרים `encoding` ו`flag` לשליטה מדויקת על הנתונים הנכתבים.

## See Also
- [Node.js fs Module documentation](https://nodejs.org/api/fs.html)
- [The Node.js fs-extra Module](https://github.com/jprichardson/node-fs-extra)
- [Understanding file encoding in Node.js](https://nodejs.org/api/buffer.html#buffer_buffers_and_character_encodings)