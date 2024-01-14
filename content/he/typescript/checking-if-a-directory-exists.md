---
title:                "TypeScript: בדיקת קיום תיקייה"
simple_title:         "בדיקת קיום תיקייה"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

במאמר זה, נטען לכם ראיונות מאויב ישירות מתמונות דיקרטוריות קיימות בהשתמש בטווח `fs.exists`. זו יכולת ייחודית שיכולה להיות מועילה כאשר בונים אתרים או יישומי דפדפן.

## איך לבדוק אם קיים דירקטוריה

ניתן לבדוק אם קיימת דירקטוריה על ידי שימוש בפונקציה `fs.exists` בתוך קוד TypeScript. להלן דוגמא של קוד לבדיקת קיומו של דירקטוריה:

```TypeScript
import * as fs from 'fs';

// שם הקובץ או הנתיב של הדירקטוריה שברצונכם לבדוק את קיומה
const directory = 'images';

fs.exists(directory, (exists: boolean) => {
  if (exists) {
    console.log(`הדירקטוריה ${directory} קיימת.`);
  } else {
    console.log(`הדירקטוריה ${directory} אינה קיימת.`);
  }
});

// פלט: הדירקטוריה images קיימת.
```

כאמור, באמצעות הפונקציה `fs.exists` ניתן לבדוק באופן יעיל אם דירקטוריה מסוימת קיימת או לא.

## מעמקים

הפונקציה `fs.exists` מוגדרת כפונקציה סינכרונית, כך שתכלול תפקידים כמו פתיחת קובצים וסגירתם, ההקמה והנתיב היחיד יהיו שמורים. פונקציה זו מקבלת כקלט נתיב לקובץ או לדירקטוריה ופונקציית ההתאמה, ומחזירה את המשתנה `בוליאני` שמציין את הקיום או החסרון של הנתיב המבוקש.

## ראו גם

- https://nodejs.org/api/fs.html#fs_fs_exists_path_callback
- https://www.typescriptlang.org/docs/handbook/2/typescript-in-5-minutes.html