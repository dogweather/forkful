---
title:                "TypeScript: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## למה

קריאת קובץ טקסט נחשבת לכישלון בתעשיית התוכנה עקב חולשת האיתור. זהו כלי חשוב לפיתוח יישומי תוכנה וכן לניתוח וניהול מידע. בכתבה הזאת נלמד איך לכתוב קוד ב-TypeScript כדי לקרוא קובץ טקסט ולהחזיר את תוכנו.

## איך לעשות את זה

מתחילים עם הכותרת המתאימה:

```TypeScript
function readTextFile(file:string): string {
// הגדרת משתנה לאחסון התוכן
let content:string = "";

// הגדרת ייבוא הספריה fs שתאפשר קריאת קבצים
import * as fs from 'fs';

// השמת הקובץ במשתנה נוסף
const fileContent = fs.readFileSync(file, 'utf-8');

// החזרת התוכן שנקרא
return content = fileContent;
}

// קריאת הפונקציה ושמירת התוכן במשתנה
const fileContent = readTextFile("example.txt");

// הדפסת התוכן
console.log(fileContent);
```

הנה כמה דוגמאות נוספות ליצירת קוד קריאת קובץ טקסט עם TypeScript:

### קריאת קובץ משמן

```TypeScript
// ייבוא הספריה fs-extra
import * as fs from 'fs-extra';

// הגדרת ייצוג הקובץ
const filePath = "/Users/user/Documents/example.txt";

// קריאת הקובץ והשמת התוכן במשתנה
const fileContent = fs.readFileSync(filePath, 'utf-8');

// הדפסת התוכן
console.log(fileContent);
```

### קריאת קובץ מהאינטרנט

```TypeScript
// ייבוא הספריה node-fetch
import fetch from 'node-fetch';

// הגדרת ייצוג הכתובת של הקובץ
const fileURL = "https://example.com/file.txt";

// קבלת התוכן מהכתובת המתוארת
fetch(fileURL)
    .then(response => response.text())
    .then(data => console.log(data));
```

השמה של תוכן קובץ למערך משתנים:

```TypeScript
// קריאת קובץ ושמירת התוכן במשתנה fileContent
const fileContent = fs.readFileSync("example.txt", 'utf-8');

// הורדת ה"שורות" של הקובץ למשתנה
const lines = fileContent.split('\n');

// הדפסת המערך הכולל את ה"שורות" של הקובץ
console.log(lines);
```

###