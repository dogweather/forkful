---
title:                "TypeScript: כתיבת המחרוזת לאותיות גדולות"
simple_title:         "כתיבת המחרוזת לאותיות גדולות"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה

השתמשות בפונקציית הכתיבה באותיות גדולות ב TypeScript יכולה לשפר את יכולת התכנות ולהוסיף קצת כיף לכתיבת קוד.

## איך לעשות

תחילה, נגדיר משתנה טקסט עם התווים שברצוננו להכניס לאותיות גדולות. לדוגמה:

```TypeScript
let text: string = "שלום לכולם!";
```

לאחר מכן, נשתמש בפונקציית הכתיבה באותיות גדולות על המשתנה ונציג את התוצאה בעזרת הפקודה "console.log". כך:

```TypeScript
console.log(text.toUpperCase());
```

פלט של הקוד מעל יהיה "שלום לכולם!" עם התווים כתובים באותיות גדולות.

## העמקה

פונקציית הכתיבה באותיות גדולות זהה לפונקציה בשם "toUpperCase" גם בשפת JavaScript. מאחר ו TypeSript מבוססת על JavaScript, ניתן להשתמש בפונקצייה זו גם ב TypeScript. כמו כן, ניתן להשתמש בפונקציה זו גם על מחרוזות עם אותיות באורך שונה ועל מחרוזות של דוגמאות באנגלית.

## ראה גם

למדידת אותיות ב TypeScript:
https://www.typescriptlang.org/docs/handbook/basic-types.html#string

לדוגמאות ב TypeScript:
https://www.typescriptlang.org/play#/example/typescript

פונקציית הכתיבה באותיות גדולות ב JavaScript:
https://www.w3schools.com/jsref/jsref_touppercase.asp