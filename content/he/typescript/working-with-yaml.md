---
title:                "עבודה עם YAML"
html_title:           "TypeScript: עבודה עם YAML"
simple_title:         "עבודה עם YAML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## למה
אנשים מתעסקים בכתיבת קוד ב- YAML כיוון שזה מאפשר להם לארגן ולנהל מידע בקוד בפורמט פשוט יותר מ- JSON או XML.

## איך לעשות זאת
תחת ה-"```TypeScript ... ```" התאים, אנו נראה כיצד לכתוב קוד YAML ולהדפיס את התוצאה בעזרת TypeScript. לדוגמה:

```
import yaml from 'js-yaml';

// יצירת אובייקט שבתוכו מילים מסוימות
const words = {
  greetings: 'שלום',
  name: 'אדם',
  age: 30,
  hobbies: ['יצירה', 'עיצוב', 'טיולים']
};

// המרת האובייקט לקוד YAML
const yamlCode = yaml.dump(words);

// הדפסת הקוד YAML למסך
console.log(yamlCode);

// תוצאה:
// greetings: שלום
// name: אדם
// age: 30
// hobbies:
// - יצירה
// - עיצוב
// - טיולים
```

## נכסף
הדף עומק כבר אנחנו כללות כיצד לכתוב קוד YAML בעזרת TypeScript, הנה כמה עובדות נוספות שיש לדעת:

- YAML נחשב לקוד קריא יותר מ- JSON ומתאים יותר לקריאה וניהול של אובייקטים מורכבים.
- הקוד YAML ניתן להיות מכיל גם נתונים מספריים, בוליאניים ואפינדיום, לא רק מחרוזות כמו ב- JSON.
- ישנם ספריות כמו "js-yaml" שמאפשרות לנו להמיר קוד YAML לאובייקט ולהפוך, כך שניתן להשתמש בקוד YAML גם בצד השרת וגם בצד הלקוח בקוד TypeScript.

## ראו גם
- דגימת קוד YAML וההמרה ל- JavaScript: https://www.npmjs.com/package/js-yaml
- איך להשתמש בקוד YAML בפייתון: https://www.datacamp.com/community/tutorials/python-yaml-tutorial