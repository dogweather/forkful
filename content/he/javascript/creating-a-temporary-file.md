---
title:                "יצירת קובץ זמני"
html_title:           "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
פניה לקובץ זמני ב-JavaScript היא ביצוע של שמירה זמנית של מידע נתון בקובץ הנוסף וחסר אבטחה. מתכנתים משתמשים בזה כדי למנוע את צורך בשמירה של מידע רגיש במסד נתונים או בזיכרון הראשי של המחשב.

## איך ל:
הנה כיצד אתה יוצר קובץ זמני ב-Javascript. יש להתקין את npm fs ו-os.
```Javascript
const fs = require('fs')
const os = require('os')

let tempDir = os.tmpdir()
let tempFile = fs.mkdtempSync(`${tempDir}/`)
```
מריצה של קוד מעלה תיצור קובץ זמני בתיקייה הזמנית של המערכת.

## צוללים עמוק
במסגרת ההיסטוריה, קבצים זמניים נוצרו מאז הימים הראשונים של המחשב האישי, כדי לאפשר שמירה מהירה של מידע ללא הפרעה לביצועים. ההתרסה הנוכחית ל-Javascript מאפשרת גמישות רבה יותר ביצירת קבצים זמניים. חלופות לקובצים זמניים כוללות את השמירה של מידע ב-memcached או במסד נתונים Redis, אך אלה מתאימים יותר לשמירה של מאגר מידע גדול.

## ראה גם
- [נושא המתחיל של Node.js File System](https://nodejs.org/api/fs.html)
- [מקום העבודה של גיטהב mkdtemp](https://github.com/nodejs/node/blob/master/test/parallel/test-fs-mkdtemp.js)

עריכה אחרונה: אחרי גרסת Node.js@16.2.0