---
title:                "עבודה עם json"
html_title:           "TypeScript: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## למה
כדי לנהל נתונים בפורמט תקין וקל לקריאה עבור מכשירים ושירותים מגוונים באינטרנט, עבודה עם JSON הוא חיונית למתכנתי TypeScript. כמו כן, מסגרת TypeScript מאפשרת לנו להיעזר במדעי הנתונים וליצור יישומים מורכבים יותר שמתמודדים עם קבצי JSON.

## איך לעשות זאת
תחילה, יש להתקין את גירסת TypeScript האחרונה על ידי הרצת פקודת ההתקנה `npm install -g typescript`. לאחר מכן, ניתן ליצור קובץ חדש בשם "example.ts" ולבצע ייבוא של מודול JSON באמצעות הפקודה `import * as json from 'json'`. לאחר מכן, ניתן להשתמש בפקודה `json.parse()` כדי להמיר טקסט קלט בפורמט JSON לפריט מסוג "object" בפינקציה `console.log()` כדי להדפיס את הפריט הממומש בעזרת גישת מפתח: ערך.`

```TypeScript
import * as json from 'json';

let exampleJson = '{"name": "John", "age": 30}'; // טקסט קלט בפורמט JSON

let parsedJson = json.parse(exampleJson); // קבלת פריט מסוג "object"

console.log(parsedJson["name"]); // 'John', הדפסת הערך המסומן "name" בפריט הממומש
```

## טיול עמוק
עבודה עם מבנה נתונים פשוט כמו JSON יכולה להיות קלה ונגישה, אך ישנם סיטואציות מורכבות יותר כאשר יצירת יישומים מתבצעת על ידי עיבוד קבצי JSON גדולים יותר. מבחינה זו, מרכיב JSON פתוח ונוח לשימוש, כגון JSON Schema ו-JSON Pointer, יכול להיות מועיל לתחום עבודה זה.

## ראה גם
- [מדריך לעבודה עם JSON ב TypeScript](https://www.digitalocean.com/community/tutorials/how-to-work-with-json-in-javascript)
- [מתי להשתמש ב-JSON Schema vs. TypeScript](https://medium.com/@mbaker_8086/when-to-use-json-schema-vs-typescript-33e1711df1e8)
- [כתיבת