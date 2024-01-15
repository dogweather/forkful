---
title:                "המרת תאריך למחרוזת"
html_title:           "TypeScript: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה

מתי כדאי לשתף ולהמיר תאריך למחרוזת בתכנות ב-TypeScript? ישנם כמה סיבות משתנות, אך בגדול, זה עשוי להיות נחוץ כאשר רוצים להציג תאריך בפורמט מסוים או לשלבו עם מחרוזות אחרות בתוך תוכנית.

## הכיצד לעשות זאת

המרת תאריך למחרוזת ב-TypeScript הינה פשוטה ונעשית באמצעות שימוש בפונקציית `toString()`. הנה דוגמא נפוצה להמרת תאריך למחרוזת והפונקציות המשתמשות בה:

```TypeScript
const date = new Date(); // תאריך נוכחי
const dateString = date.toString(); // מחרוזת המכילה את התאריך לפי הפורמט האנגלי הנפוץ
console.log(dateString);
// פלט מצפין את התאריך כ: Sun Sep 05 2021 15:10:00 GMT+0300 (Israel Daylight Time)
```

משמעות הפרמטר `γlocal` בפונקציית `toString()` היא לאפשר למשתמש להגדיר את תאריך המחרוזת לפי אזור הזמן מקומי שלו. כדי לעשות זאת, יש להשתמש בפרמטר `local` בתוך הפונקציה, כמו בדוגמא הבאה:

```TypeScript
const date = new Date();
const dateString = date.toString('local');
console.log(dateString);
// פלט מציג את התאריך כ: 08/09/2021 15:10:20 (עבור אזור הזמן המקומי)
```

כדי לקבל מחרוזת עם שני הצורות, ניתן להשתמש בפונקציות `toLocaleDateString()` ו-`toLocaleTimeString()`, כפי שמופיע בדוגמא הבאה:

```TypeScript
const date = new Date();
const dateString = date.toLocaleDateString();
const timeString = date.toLocaleTimeString();
console.log(`${dateString}, ${timeString}`);
// פלט מציג את התאריך והשעה כממחרוזת אחת, בפורמט המאוחד: 08/09/2021, 15:10:20
```

כמובן שניתן להתאים את הפורמט ולייצר מחרוזת בדיוק כפי שרוצים, באמצעות הפונקציות השונות שמצויות ב-TypeScript.

## מעמקים

התהליך של המ