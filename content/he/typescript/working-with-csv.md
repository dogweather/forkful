---
title:                "עבודה עם קבצי CSV"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?
CSV זה פורמט קובץ טקסט שכולל נתונים מופרדים בפסיקים. מתכנתים משתמשים בו כי זה פורמט פשוט ונפוץ לייצוא ויבוא נתונים מדאטהבייסים, גיליונות עבודה וממערכות אחרות.

## איך לעשות:
כדי לעבוד עם קבצי CSV ב-TypeScript, תוכל להשתמש בחבילה כמו `csv-parse`. דוגמה בסיסית:

```TypeScript
import { parse } from 'csv-parse/sync';

const csvContent = `name,age
Alice,30
Bob,25`;

const records = parse(csvContent, {
  columns: true,
  skip_empty_lines: true
});

console.log(records);
```

דוגמת פלט:
```
[ { name: 'Alice', age: '30' }, { name: 'Bob', age: '25' } ]
```

## עיון מעמיק:
CSV (Comma-Separated Values) נפוץ מאז שנות ה-70. קיימים פורמטים אחרים כגון JSON או XML שהם יותר מרובי מאפיינים. ספריות שונות ב-TypeScript מטפלות בחיתוך נתונים ובמפתח נתונים מורכבים, ויש לכל אחת מורכבויות ופיצ'רים שונים.

## ראה גם:
- [`csv-parse` documentation](https://csv.js.org/parse/)
- [`papaparse` חבילה חלופית לעיבוד CSV](https://www.papaparse.com/)
- [CSV on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array#Iteration_methods) - מדריך על פורמט CSV.
