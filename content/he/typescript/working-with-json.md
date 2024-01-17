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

## מה ולמה? 
עבודה עם JSON היא תהליך שבו מתכנתים מתאימים נתונים בפורמט JSON. פעולה זו מאפשרת לנו לארגן ולשמור מידע בצורה נוחה ומקומית בקוד שלנו.

## כיצד לעשות זאת: 
```TypeScript
// כל עובד עם JSON מתחיל עם הגדרת קובץ JSON בתוך משתנה מסוג String.
let jsonString: string = '{"name":"John", "age":35, "occupation":"developer"}';

// כעת, נשתמש בפעולת JSON.parse כדי לקבל אובייקט מהטקסט שלנו.
let person = JSON.parse(jsonString);

// לשינוי אובייקט לתוך טקסט, נשתמש בפעולת JSON.stringify.
let newJsonString: string = JSON.stringify(person);

// כעת, הטקסט שלנו יכיל את השינויים שביצענו באובייקט.
console.log(newJsonString);
// Output: {"name":"John", "age":35, "occupation":"developer"}
```

## חקר עמוק: 
עבודה עם JSON נפוצה בעיקר ביישומי רשת אשר משתמשים בה כדי לשלוח ולקבל נתונים. בעבר, פורמט XML היה שכיח כמו פתרון לבעיות של פיצול נתונים. אך JSON הוא פתרון פשוט יותר ומתקבל בכל הפלטפורמות. בנוסף, ישנם פתרונות ניתוח נתונים נוספים כמו YAML ו-CSV אך הם פחות נפוצים בעקבות הפשטות של JSON. בפרויקטים גדולים יותר, ניתוח נתונים מומלץ לצד השימוש בביבליות ניתוח נתונים כמו JSON.net.

## ראה גם: 
עבודה עם JSON היא חלק חשוב מתכנות בפלטפורמת TypeScript. לכן כדאי לקרוא על פעולות ופנקציות נוספות שניתן להשתמש בהם כדי להפקיד ולעבוד עם נתונים בפורמט JSON:

- תיעוד JSON רשמי של TypeScript: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-9.html
- פונקציות נוספות לעבודה עם JSON ב-TypeScript: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-9.html#new-json-apis
- ספריית JSON.net לניתוח ויצירת נתוני JSON: https://www.newtonsoft.com/json