---
title:                "עבודה עם קובץ csv"
html_title:           "TypeScript: עבודה עם קובץ csv"
simple_title:         "עבודה עם קובץ csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## למה

עבודה עם קבצי CSV היא תהליך חשוב בהתפתחות תוכניות ממוחשבות ואפליקציות. בעזרת TypeScript, התכנות בשפת סקריפטים יכול להיות קל ומהיר להתאים את הנתונים מקובץ CSV לתבניות ומבני נתונים נדרשים.

## איך לעשות זאת

לפניכם דוגמאות לקוד ב-TypeScript המפעילות כמה חישובים על קובץ CSV ומייצרות פלט מתאים. הקוד מוצג בתוך קטעי קוד אשר מוקפים בסימון "```".

```typescript
// קבלת הנתונים מקובץ CSV
const csvData = `
  Name, Age, Country
  John, 28, USA
  Maria, 32, Brazil
  David, 24, Canada
`;

// סידור הנתונים לתבנית של מערך בתוך מערך
const formattedData: any[][] = csvData
  .trim()
  .split("\n")
  .map(row => row.split(","));

// חישוב ממוצע גילו של האנשים בנתונים
const avgAge = formattedData
  .slice(1)
  .reduce((acc, curr) => acc + parseInt(curr[1]), 0) / (formattedData.length - 1);

// פלט: ממוצע גילים = 28
console.log(`ממוצע גילים = ${avgAge}`);
```

## מעמקים

עבודה עם קבצי CSV באמצעות TypeScript משתלבת בצורה טבעית עם פעולות יסודיות להפעלת קבצים ותיעוד ניתן למציאות כדי להסביר העדפה נוספת בהשתמש בכלי זה עבור הפעלת קבצי CSV.

כמו כן, הקוד שנכתב ב-TypeScript יכול לבדוק את הנתונים המעובדים, כך שתוכלו להיות בטוחים בהגשת פלט מדויק ומסודר.

## ראו גם

עבודה עם קבצי CSV בשפת TypeScript היא רק אחד מיישומי השפה המגוונים ומועילים. אם תרצו להתחיל ללמוד על תכנות ב-TypeScript יש לכם כמה מקורות נהדרים להתחיל עם:

- [מסמכי המדריכים של TypeScript](https://www.typescriptlang.org/docs/home.html)
- [ספר