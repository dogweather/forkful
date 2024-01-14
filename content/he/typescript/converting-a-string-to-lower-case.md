---
title:    "TypeScript: כתיבת טור אותיות לאותיות קטנות"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## למה

כתיבת קוד בפחות קשה כשמנסים להשתמש בפונקציית המרה של טיפוסקריפט להמיר מחרוזת לאותיות קטנות. בנוסף, זה עוזר לשמור על קוד נקי וקל לקריאה על ידי הפנייה לאותו צורך.

## איך לעשות זאת

תחילה, השתמשו בפונקציית המרה של טיפוסקריפט על המחרוזת המבוקשת עם סימן תחילי מצביע (# על ידי שימוש בשורת הפקודה `toLowerCase()` כפי שנראה בדוגמה הבאה:

```TypeScript
let str = "HELLO WORLD";
let lowerStr = str.toLowerCase(); 
```

כעת, אם תדפיסו את המשתנה `lowerStr`, תקבלו את הפלט הבא:

```TypeScript
console.log(lowerStr);

// פלט: hello world
```

## חפירה עמוקה

כאשר משתמשים בפונקציית המרה לאותיות קטנות, חשוב לדעת שהיא משתמשת באלגוריתם פשוט של הורדת הערך ההספרי של כל אות במחרוזת. כך ש, למשל, אם ישנה מחרוזת עם אותיות בגודל קטן וגדול ביחד, המספרים של האותיות יהיו זהירים יותר. נסו להמיר את המחרוזת הבאה לאותיות קטנות ובדקו אם אתם מקבלים את התוצאה הרצויה:

```TypeScript
let str2 = "AbCdEfGhIjKlMnOpQrStUvWxYz";
let lowerStr2 = str2.toLowerCase(); 
console.log(lowerStr2);

// תוצאה: abcdefghijklmnopqrstuvwxyz
```

## ראו גם

- [למדו עוד על פונקציית המרה של טיפוסקריפט](https://www.typescriptlang.org/docs/handbook/utility-types.html#string-manipulation-types)
- [תיאור פונקציית המרה בתיעוד של טיפוסקריפט](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html#inferring-type-information-from-template-literal-types)