---
title:                "TypeScript: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## למה

למה לכתוב קובץ טקסט בכלל? המניע העיקרי לכתיבת קובץ טקסט הוא לייצר מידע או לשמור מידע שכבר קיים לשימוש מתוכנתים אחרים. ייתכן גם שתוכל להשתמש בקובץ טקסט כחלק מקוד אחר או כדי לשתף מידע עם אנשים אחרים.

## איך לעשות זאת

כתיבת קובץ טקסט בטיפוסקריפט היא פשוטה ונוחה. באמצעות הפעולות המתאימות והפונקציות, תוכל לכתוב ולקרוא קבצי טקסט בקלות. הנה דוגמאות לכתיבת וקריאת קובץ טקסט בטיפוסקריפט:

```typescript
// כתיבה לקובץ טקסט חדש
const fs = require('fs');
fs.writeFile('myFile.txt', 'Hello, world!', function (err) {
  if (err) throw err;
  console.log('Successfully wrote to file!');
});

// קריאת קובץ טקסט קיים והצגת התוכן
fs.readFile('myFile.txt', 'utf8', function (err, data) {
  if (err) throw err;
  console.log('File content: ', data);
});
```

פלט:

```shell
Successfully wrote to file!
File content: Hello, world!
```

אם תרצה להוסיף תוכן לקובץ טקסט קיים, תוכל להשתמש בפונקציית `appendFile()` כך:

```typescript
const fs = require('fs');

// נוסף לקובץ טקסט קיים
fs.appendFile('myFile.txt', 'This is an added line.', function (err) {
  if (err) throw err;
  console.log('Successfully added to file!');
});
```

פלט:

```shell
Successfully added to file!
```

ניתן גם למחוק קובץ טקסט בטיפוסקריפט באמצעות הפונקציה `unlink()` כך:

```typescript
const fs = require('fs');

// מחיקת קובץ טקסט קיים
fs.unlink('myFile.txt', function (err) {
  if (err) throw err;
  console.log('Successfully deleted file!');
});
```

פלט:

```shell
Successfully deleted file!
```

## חקירה מעמיקה

כתיבת וקריאת קובץ טקסט היא פעולה חשובה בתכנות ובעבודה עם מידע. למרבה המזל, טיפוסקריפט