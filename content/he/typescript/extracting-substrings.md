---
title:                "חילוץ תת-מחרוזות"
html_title:           "Bash: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?

חילוץ תת-מחרוזות הוא פרק של מחרוזת מתוך מחרוזת אחרת. מתכנתים משתמשים בזה שכזה נותן להם את היכולת להתמקד במחרוזת מקטנה יחסית מתוך המחרוזת הארוכה.

## איך ל־:

נשתמש בJavaScript function `substring` או `slice` כדי לחלץ תת-מחרוזות בTypeScript:

```TypeScript 
let txt = "בית הקפה המקומי";
console.log(txt.substring(0, 4));  // Outputs: "בית"
console.log(txt.slice(-3));  // Outputs: "מקומי"
```

## צלילה עמוקה:

גילויים טכניים גאוניים לפני שנים רבות אפשרו לנו לחלץ תת-מחרוזות באמצעות `substring` ו`slice`. שני השיטות האלה הן די דומות, אך יש לההן הבדלים מקטינים. `substring` לא מתמודדת טוב עם ארגומנטים שליילים, בעוד `slice` כן.

(MDN הוא מקור מצוין אם אתה מעונין לקרוא יותר על `substring` ו`slice`.)

## גם ראה:

1. [MDN - Substring method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
2. [MDN - Slice method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)