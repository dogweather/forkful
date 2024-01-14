---
title:                "Javascript: עבודה עם YAML"
simple_title:         "עבודה עם YAML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## למה

JavaScript נחשב לשפת תכנות עילאית ואפשריויותיו הרבות והגמישות שלו גורמות לו להיות פופולרי עם מפתחים רבים ברחבי העולם. מסגרת YAML מציעה כלי מושלם לפיתוח באמצעות JavaScript ונותנת למפתחים הנשק האידיאלי עבור כתיבת קוד נקי ומסודר.

## איך לעבוד עם YAML ב-JavaScript

כתיבת קוד YAML ב-JavaScript היא פשוטה ומתבצעת על ידי התאמה של הערכים למבנה הנכון עם מבני הנתונים. לדוגמה, נראה כיצד ליצור רשימה בעזרת YAML ב-JavaScript:

```javascript
var list = [
  {
    name: 'John',
    age: 30
  },
  {
    name: 'Jane',
    age: 25
  },
  {
    name: 'Bob',
    age: 35
  }
];

console.log(list);
```

הפלט שלנו יהיה:

```javascript
[ { name: 'John', age: 30 },
  { name: 'Jane', age: 25 },
  { name: 'Bob', age: 35 } ]
```

האם קל לא משהו? כעת, נראה כיצד ניתן לכתוב קובץ YAML ב-JavaScript בעזרת הספרייה המובנית `yamljs`:

```javascript
var yaml = require('yamljs');
var data = { name: 'John', age: 30 };

var yamlString = yaml.stringify(data);
console.log(yamlString);
```

הפלט שלנו יהיה:

```
name: John
age: 30
```

כעת יש לנו קובץ YAML חדש המכיל את הערכים שלנו במבנה מתאים.

## חקירה מעמיקה

YAML הינו פורמט מבנה נתונים פשוט וברור אשר מאפשר למפתחים לבנות קבצי תצורה בצורה קלה ומדוייקת. ניתן להשתמש ב-YAML בכמה שוני שימושים בפיתוח JavaScript, כולל כתיבת קובץי תצורה, מסמכי מידע והגדרות. בנוסף, מסגרת YAML מציעה תמיכה בדוגמאות קוד המאפשרות למפתחים לעבוד בצורה נקייה ומסודרת.

## ראו גם

- תיעוד הספרייה `yamljs`: https://www.npmjs.com/package/yamljs
- YAML מדריך למתח