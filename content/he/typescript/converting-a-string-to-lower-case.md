---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?

המרת מחרוזת לאותיות קטנות היא פעולה שבה כל האותיות הגדולות במחרוזת ממירות לאותיות קטנות. מתכנתים בוחרים לעשות זאת כדי להבטיח עקביות בהשוואות מחרוזות, או לניתוח נתונים.

## איך לעשות:

הנה דוגמא לקוד TypeScript שממיר מחרודת לאותיות קטנות:
```TypeScript
let str = "Hello, TypeScript!";
let lowerCaseStr = str.toLowerCase();
console.log(lowerCaseStr); // יוצא "hello, typescript!"
```

בקוד הזה, אנחנו מפעילים את המתודה `.toLowerCase()` על המחרוזת `str` ושומרים את התוצאה במשתנה `lowerCaseStr`.

## צלילה מעמיקה:

המרת מחרוזת לאותיות קטנות היא טכניקה ותיקה בתכנות, שמוכרחת בשל גמישות ההקלדה. בחלק מהשפות יש דרכים אחרות לבצע זאת, אבל ב-TypeScript `.toLowerCase()` היא הפופולרית ביותר.

אם אתם רוצים לממש את זה בעצמכם, תצטרכו לדעת את קודיות ASCII של האותיות ולהשתמש במתודה של TypeScript `.charCodeAt`.

## ראה גם:

לפרטים נוספים, בקרו באתרי האינטרנט הבאים:

- [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [TypeScript Documentation](https://www.typescriptlang.org/docs/)
- [JavaScript Info](https://javascript.info/)