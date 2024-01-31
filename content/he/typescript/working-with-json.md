---
title:                "עבודה עם JSON"
date:                  2024-01-19
simple_title:         "עבודה עם JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?
JSON, שמעתם על זה, נכון? זה פורמט התעתיק שלנו למידע בפרויקטים ויישומים. אנחנו משתמשים בו כי זה קריא, קל לניתוח ורווחי בכל הפלטפורמות.

## איך לעשות:
קוד מדגים ותוצאות לדוגמא.

```TypeScript
// המרת אובייקט ל-JSON string.
const user = { name: 'דוד', age: 30 };
const jsonUser = JSON.stringify(user);
console.log(jsonUser); // יחזיר: '{"name":"דוד","age":30}'

// המרת JSON string בחזרה לאובייקט.
const jsonData = '{"name":"דוד","age":30}';
const userObject = JSON.parse(jsonData);
console.log(userObject.name); // יחזיר: דוד
```

## צלילה עמוקה
JSON (JavaScript Object Notation) הוא שיטה שהומצאה כחלק משפת JavaScript, אבל כיום היא תקן מתוקשר לעבודה עם נתונים בכמעט כל שפת תכנות. ישנם אלטרנטיבות כמו XML, יחסיות ופחות נוחות לשימוש. כשאנחנו עובדים עם JSON ב-TypeScript, אנחנו לעיתים צריכים להגדיר טיפוסים על מנת להבטיח תקינות הנתונים.

## ראו גם
- [MDN Web Docs on JSON](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
- [JSON Schema](http://json-schema.org/) - להגדיר סכמות ולוודא תקינות של JSON
