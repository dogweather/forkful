---
title:                "השוואת שתי תאריכים."
html_title:           "TypeScript: השוואת שתי תאריכים."
simple_title:         "השוואת שתי תאריכים."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה

מוטיבציה להשוואה של שתי תאריכים ניתן להיות מספרת. לדוגמא, כאשר אנו רוצים לסנן נתונים על ידי תאריך או ליצור לוגיקת עסקת של לוגינג במערכת שלנו, יכולה להיות חיונית להשוות תאריכים כדי להבין איזו תאריך הינה בפני בדיוק ולפעול בהתאם.

## איך לעשות זאת

### ייבוא מודולים ויצירת אובייקטים תאריך

כדי לבצע השוואה בין שני תאריכים בטיפוסיפט, תחילה עלינו ליצור אובייקטים תאריך כלשהו. נוכל לעשות כך על ידי ייבוא המודול "date-fns" ושימוש בפונקציות שלו כדי ליצור את התאריך הרצוי. לדוגמא, אם נרצה ליצור את התאריך הנוכחי, נשתמש בפונקציה `new Date()`.

```TypeScript
import { format, isEqual } from 'date-fns';

const today = new Date();
const date1 = new Date('2021-01-01');
const date2 = new Date('2021-05-05');

console.log(isEqual(date1, date2)); // false
console.log(isEqual(today, date2)); // true
```

### שימוש בפונקציות מובנות של תאריכים

טיפוסקריפט מציע פונקציות מובנות נוספות לפעולות על תאריכים, כגון `getTime()` שמחזיר את מספר המילי-שניות מתאריך התחלת התגליות (01/01/1970 00:00:00). ניתן להשתמש בפונקציות אלה כדי לבצע השוואה בין שני תאריכים בצורה מהירה ויעילה.

```TypeScript
const date1 = new Date('2021-01-01');
const date2 = new Date('2021-05-05');

console.log(date1.getTime() === date2.getTime()); // false
```

## לחקור עומק יותר

על מנת להבין איך פונקציות השוואה של תאריכים עובדות בשפת טיפוסקריפט, כדאי להתעמק קצת יותר בנושא. בנוסף, כדאי להתבונן בפ