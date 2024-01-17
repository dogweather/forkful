---
title:                "מציאת אורך של מחרוזת"
html_title:           "TypeScript: מציאת אורך של מחרוזת"
simple_title:         "מציאת אורך של מחרוזת"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

מה ולמה?
מציאת אורך של מחרוזת היא כלי חשוב בתכנות המאפשר לנו להבין כמה תווים יש במחרוזת תווים. מתכנתים עושים זאת כדי לעבוד עם מחרוזות ולהציג נתונים בצורה אינטואיטיבית.

איך לעשות?
כדי למצוא את אורך המחרוזת ב-TypeScript, ניתן להשתמש בפונקציה המובנית .length. לדוגמה:

```
let str = "שלום עולם";
console.log(str.length);
```
הפלט יהיה:
```
9
```

עומק השטח
מציאת אורך של מחרוזת הוא כלי שנמצא כבר די הרבה שנים ונכתב לראשונה בשפת תכנות דרכדט שנוצרה בשנת 1964. אם אתם מחפשים אלטרנטיבות לפונקציה length, ב-TypeScript ניתן למצוא גם את המתודה .size() שמשמשת לאותו מטרה.

ראה גם
למידע נוסף על מציאת אורך של מחרוזות ב-TypeScript, ניתן לבדוק את הקישורים הבאים:
- מידע נוסף על הפונקציה .length: https://www.typescriptlang.org/docs/handbook/basic-types.html#string
- השוואה בין .length ל-.size(): https://stackoverflow.com/questions/21243970/difference-between-size-method-and-length-property-in-javascript