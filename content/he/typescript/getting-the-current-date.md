---
title:                "קבלת התאריך הנוכחי"
html_title:           "TypeScript: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?

הפעלת תאריך נוכחי היא פעולה שתכונן ותחזיר את התאריך והשעה הנוכחיים. תהליך זה חשוב עבור מתכנתים כדי ליישם תאריך ושעה ביישומיים שונים, כמו יישומי לוח שנה, אירועים ועוד.

## כיצד לעשות זאת:

כדי לקבל את התאריך והשעה הנוכחיים בטיפוסקריפט, ניתן להשתמש במאפייני התאריך המובנים בטיפוסקריפט או ליצור משתנה מטיפוס תאריך ולהשתמש בפונקציות מובנות כמו getDate(), getMonth(), getFullYear() וכו '. הנה דוגמה:

```TypeScript
// משתנה מטיפוס תאריך
let today: Date = new Date();
// משתנה לשעה נוכחית
let currentTime: number = today.getHours();
console.log(currentTime);
```

פלט: 13

## חקירה מעמיקה:

בעבר, קבלת תאריך ושעה הייתה פעולה מורכבת יותר עם צורך בשימוש בספריות חיצוניות. במקרים שבהם נדרשת חישובית יותר מדוייקת, ניתן להשתמש בקביעה ישירה של התאריך באמצעות חישובים מתמטיים. ישנן גם אפשרויות נוספות כמו שימוש ב-API של תאריך קבוע לעבודה עם תאריך ושעה בצורה מקורית ומדוייקת יותר. בסופו של דבר, זה כל כך קל ונוח להשתמש בפונקציות המובנות בטיפוסקריפט כדי לקבל את התאריך והשעה הנוכחיים.

## ראה גם:

למידע נוסף על פעולות עם תאריך ושעה בטיפוסקריפט, ניתן להציץ במדריכים הבאים:

- [תיעוד משתני התאריך המובנים בטיפוסקריפט](https://www.typescriptlang.org/docs/handbook/utility-types.html#date)
- [API של תאריך קבוע בטיפוסקריפט](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DateTimeFormat)