---
title:                "TypeScript: יצירת קובץ זמני"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה
יצירת קובץ זמני הוא כלי חשוב בתכנות TypeScript כאשר נרצה ליצור ולשנות קבצים בעת הריצה של התוכנית. תוכלו להשתמש בקבצים זמניים כדי להתאים את התוכנית שלכם לסביבה הנוכחית או לשמור על ניתובים מיוחדים.

## איך ליצור קובץ זמני

תחילה, נצטרך להתקין את הספרייה "fs" מתוך הספרייה הפנימית של טיפוסקריפט.

```TypeScript
import * as fs from 'fs';
```

כדי ליצור קובץ זמני, נבחר את השם של הקובץ ונגדיר את הנתיב שלו על ידי השתמשות בפונקציית "tmpfile()" של הספרייה "fs".

```TypeScript
const filename: string = 'temp.txt';
const filepath: string = fs.tmpfile(filename);
```

אחרי שיצרנו את הנתיב, נשתמש בפונקציית "writeFileSync()" כדי לכתוב לקובץ זמני טקסט שתצייןו.

```TypeScript
fs.writeFileSync(filepath, 'Hello World!');
```

כעת, ניתן לקרוא ולערוך את הקובץ כרגיל.

```TypeScript
const data: string = fs.readFileSync(filepath);
console.log(data);
// פלט: Hello World!
```

## מעמד עמוק

כאשר ניצור קובץ זמני, הספרייה fs תיצור נתיב לקובץ זמני באופן אקראי בתיקיה המכילה את הקוד הנוכחי. נתיב זה ישתנה בכל פעם שתפעילו את הפונקציה "tmpfile()". זה מבטיח שנוכל ליצור כמה קבצים זמניים שנרצה עם שמות שונים בכדי לבדוק את התוכנית שלנו.

כדי למחוק את הקובץ הזמני כאשר אנחנו סיימנו להשתמש בו, אנחנו ניתן להשתמש בפונקציית "unlinkSync()" של הספרייה "fs".

```TypeScript
fs.unlinkSync(filepath);
```

תמיד יהיה טוב לבדוק את "נתיב קובץ זמני" כדי לוודא שהייתה הדמייה נכונה.

## ראו גם