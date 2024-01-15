---
title:                "קריאת קובץ טקסט"
html_title:           "TypeScript: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## למה

למה אדם ירצה לקרוא קובץ טקסט? כי זהו כלי חשוב בתכנות בשפת TypeScript וניתן להשתמש בו כדי לעבוד עם מידע מגוון כגון קבצי תוכנית, נתונים טבלאיים ועוד.

## איך לעשות זאת

כדי לקרוא קובץ טקסט בשפת TypeScript, ישנם מספר שלבים פשוטים אשר תצטרכו לבצע:

```TypeScript
// צעד ראשון: ייבא את המודול "fs" מתוך הספרייה המובנית של Node.js
import * as fs from 'fs';

// צעד שני: הגדירו את שם הקובץ, הכולל את הנתיב המלא לקובץ
const fileName = "./example.txt";

// צעד שלישי: קראו את הקובץ והציבו את התוכן שלו בתוך משתנה
const fileContent = fs.readFileSync(fileName, "utf-8");
```

כעת, אם תרצו להדפיס את התוכן של הקובץ שקראתם, תוכלו להשתמש בפקודה הבאה:

```TypeScript
console.log(fileContent);
```

כשתפעילו את הקוד, תראו בקונסול מספר השורות והמילים שנמצאות בקובץ הטקסט.

## העמקת הבנת הנושא

הקבצים הטקסט הם דרך מושלמת לאחסן ולהשתמש במידע מגוון בתכנות בשפת TypeScript. אפשר למצוא פתרונות רבים לקריאה של קבצי טקסט, והפתרונות הללו כוללים שימוש בפונקציות כמו readFileSync וגם שימוש בכלים צד שלישי כמו ספריית fs-extra. שימוש ב-readFile ו-readFileSync דורש התכנסות לטיפוסים המתאימים של המשתנים כדי לקרוא את הקובץ באופן תקין.

## ראו גם

- [מדריך מפורט יותר על קריאת קבצי טקסט בשפת TypeScript](https://www.digitalocean.com/community/tutorials/reading-files-with-nodejs)
- [החלפת תוכן בקובץ טקס