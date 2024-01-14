---
title:                "TypeScript: השוואת שתי תאריכים"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

**מדוע:**

השוואת תאריכים היא כלי חשוב בתכנות של TypeScript, שמאפשר לנו לבדוק את היחס בין שני תאריכים ולהתאים לפי כך את התכנית שלנו. השוואת תאריכים יכולה לסייע לנו בהרבה מצבים, כגון בבדיקת תקינות של תאריכים או ביצירת תנאים לביצוע פעולות בתאריך מסוים.

**כיצד להשתמש:**

ניתן להשוות תאריכים באמצעות הפונקציה "compare" שמקבלת שני תאריכים כפרמטרים. לדוגמה, ננסה להשוות את התאריך הנוכחי לתאריך התפקיד שלנו:

```TypeScript
let currentDate = new Date();
let roleStartDate = new Date(2021, 0, 1); // 1 בינואר 2021

let comparisonResult = compare(currentDate, roleStartDate);
console.log(comparisonResult); // 1
```

במקרה זה, נקבל כתוצאה את המספר 1, המסמל שהתאריך הנוכחי הינו גדול יותר מתאריך התחילת התפקיד שלנו.

ניתן להשתמש גם בפונקציות מובנות נוספות כמו "isBefore" ו-"isAfter" עבור השוואה מפורטת יותר של תאריכים. לדוגמה, נבדוק אם התאריך הנוכחי נמצא לפני תאריך ספציפי:

```TypeScript
let currentDate = new Date();
let comparisonDate = new Date(2022, 6, 1); // 1 ביולי 2022

let isBeforeComparisonDate = isBefore(currentDate, comparisonDate);
console.log(isBeforeComparisonDate); // true
```

**עומק הנסיעה:**

בנוסף לפונקציות המובנות, ישנם גם חבילות חיצוניות שמתמחות בהשוואת תאריכים באופן מתקדם יותר. למשל, חבילת "date-fns" מציעה מגוון רחב של פונקציות להתייחסות ויצירת תאריכים, כולל השוואות ובדיקות תקינות.

בנוסף, ישנם ספריות נוספות שניתן להשתמש בהן עבור השוואת תאריכים, תל