---
title:                "TypeScript: כתיבה לפלט שגיאות סטנדרטי"
simple_title:         "כתיבה לפלט שגיאות סטנדרטי"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## למה

למה לכתוב לפלט שגיאות סטנדרטי? משתמשים בכתיבה לפלט שגיאות סטנדרטי כדי לעזור להבין ולאתר בעיות בתכנית שלהם. כאשר ניתן נתונים מפורטים על טעויות כלשהן, המפתחים יכולים לתקן אותן ולהמשיך לפתח את התכנית בצורה יעילה יותר.

## איך לכתוב לפלט שגיאות סטנדרטי

תחילה ניצור תיקיית פרויקט חדש עם קוד TypeScript מתחת ```TypeScript``` נרשום את הפונקציה ```console.error()``` המאפשרת לנו לכתוב לפלט שגיאות סטנדרטי. לדוגמה:

```typescript
let num: number = 12;

if (num > 10) {
  console.error("Number is greater than 10.");
} else {
  console.log("Number is less than 10.");
}
```
פלט של הקוד הזה יהיה:

```
Number is greater than 10.
```

כמו כן, ניתן להשתמש בתנאים נוספים כדי להכתיב לפלט שגיאות ספציפיות, לדוגמה:

```typescript
function divide(x: number, y: number) {
  if (y === 0) {
    console.error("Cannot divide by zero.");
  } else {
    return x / y;
  }
}

console.log(divide(10, 2)); // Output: 5
console.log(divide(10, 0)); // Output: Cannot divide by zero.
```

## מעמקים נמוכים

בכתיבת לפלט שגיאות סטנדרטי ישנם אפשרויות רבות לניתוח הפלט הנוצר ולקבלת נתונים נוספים. בדרך כלל, מפתחים משתמשים בפונקציות כמו: ```console.clear()``` כדי לנקות את המסך, ו-```console.trace()``` כדי להיכנס לתחום הפונקציה או המתודה שגרמה לכתיבת השגיאה ספציפית. ניתן גם להשתמש בפונקציה ```console.assert()``` עם תנאי, המאפשרת לנו לבדוק תנאים במהלך התכנות ולכתוב לפלט שגיאות אם הם לא מתקיימים.

## ראה גם

למידע נוס