---
title:                "לקבל את התאריך הנוכחי"
html_title:           "TypeScript: לקבל את התאריך הנוכחי"
simple_title:         "לקבל את התאריך הנוכחי"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

אפשר לגלות את התאריך הנוכחי בעזרת לשני аשורשים קלים של קוד תלת-חבוי, ההתמקדות שלו היא שימושי לארגונים ופרויקטים שרוצים לפתח אפליקציות אסינכרוניות ותוכניות מורכבות בנוסף לבדיקת שעה.

## How To

השימוש בפונקציית `Date()` בסיסי ומראה את התאריך הנוכחי באמצעות מרכיבי היום, החודש והשנה בפורמט DD/MM/YYYY.

```TypeScript
const currentDate = new Date();
console.log(currentDate.toLocaleDateString('he-IL'));
```

Output:
23/12/2021

כדי להציג את התאריך המלא באופן מלא כולל השעה והדקות, ניתן להשתמש במתודת `toLocaleString()` ולהעביר לה את השפה כפרמטר.

```TypeScript
console.log(currentDate.toLocaleString('he-IL'));
```

Output:
23/12/2021, 18:36:53

## Deep Dive

בנוסף לפונקציות התאריך השונות שכבר נדונו, קיימות עוד אפשרויות לקבלת מידע תאריך ושעה נוספים. למשל, אם רוצים לקבל את היום הראשון בחודש, ניתן להשתמש במתודה `getDate()` בתוך מתודת `getMonth()`.

```TypeScript
console.log('The first of the month is: ' + new Date(currentDate.getFullYear(),currentDate.getMonth(),1).toLocaleDateString('he-IL'));
```

Output:
The first of the month is: 01/12/2021

כדי לקבל את השנה הבאה בפורמט מלא, ניתן להשתמש במתודה `getFullYear()` ולהוסיף את הערך 1. ניתן גם להשתמש במתודה `toString()` כדי להציג את התאריך בפורמט טקסט.

```TypeScript
console.log('The next year is: ' + (currentDate.getFullYear()+1).toString());
```

Output:
The next year is: 2022

## See Also

- [MDN - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript Official Documentation](https://www.typescriptlang.org/)
- [JavaScript.info - Date and Time](https://javascript.info/date)