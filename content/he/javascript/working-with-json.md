---
title:                "עבודה עם JSON"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם JSON"
simple_title:         "עבודה עם JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?
JSON, שזה ראשי תיבות של JavaScript Object Notation, הוא פורמט לשיתוף נתונים. תכניתנים משתמשים בו כי הוא פשוט, קריא ותואם רוב השפות ופלטפורמות התכנות.

## איך עושים את זה:
קוד להמרת אובייקט ל-JSON וחזרה:
```Javascript
// להמרת אובייקט ל-JSON
const object = { name: 'דני', age: 30 };
const jsonString = JSON.stringify(object);
console.log(jsonString); // ידפיס {"name":"דני","age":30}

// להמרת JSON לאובייקט
const jsonObject = JSON.parse(jsonString);
console.log(jsonObject); // ידפיס { name: 'דני', age: 30 }
```

## צלילה לעומק
JSON נוצר בשנת 2001 על ידי דאגלס קרוקפורד. הפורמט מבוסס על תחביר של JavaScript אך תומך גם בשפות אחרות דרך ספריות ייעודיות. רבים בוחרים ב-JSON על פני XML מכיוון שהוא פחות מסורבל ויותר מהיר בפעולה. עם זאת, תמיכה בהערות ו-namespaces מוגבלת, מה שכן נתמך ב-XML.

## גם כדאי לראות
- מדריך מעמיק על JSON באתר MDN: https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON
- ספריית JSON בשפות תכנות שונות: https://www.json.org/json-en.html
- תקנים ומפרטי JSON: http://www.ecma-international.org/publications/standards/Ecma-404.htm
