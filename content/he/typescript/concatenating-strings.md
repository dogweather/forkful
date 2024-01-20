---
title:                "חיבור מחרוזות"
html_title:           "C++: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה ולמה?
השרשור של מחרוזות הוא התהליך שבו אנו מחברים שתי מחרוזות או יותר למחרוזת אחת. מתכנתים עושים את זה למגוון של סיבות, מהתאמה לצורך השליחה של נתונים כטקסט.

## איך לעשות:
הנה דוגמה של קוד שמשרשר מחרוזות ב-TypeScript:

```TypeScript
let str1: string = "שלום, ";
let str2: string = "עולם!";
let str3: string = str1.concat(str2);
console.log(str3);  // "שלום, עולם!"
```

## הצצה לעומק:
שיטת שרשור מחרוזות התפתחה הרבה לפני שפת התכנות טייפסקריפט. היא נעשתה מקובלת בשפות רבות מאז השנים הראשונות של התכנות. בטייפסקריפט, ישנם דרכים נוספות לעבוד עם מחרוזות מלבד `concat()`, כולל באמצעות תו האסוציאציה (`${}`). יתרה מכך, TypeScript מתאים למערכת סוג של JavaScript, שהיא לא ניתן לסוג, ומאפשרת גמישות רבה בטיפול במחרוזות.

## ראה גם:
- [String ב- MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- [TypeScript ב- MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/TypeScript)