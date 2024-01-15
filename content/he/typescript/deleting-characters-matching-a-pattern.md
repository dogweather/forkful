---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "TypeScript: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה

אנשים משתמשים במכתבים כדי למצוא ולמחוק תווים התואמים לתבנית כדי לשפר את הקוד ולהפחית בעיות בעת ריצה.

## איך לעשות

```TypeScript
const input: string = "hello123";
const pattern: RegExp = /[0-9]/g;
const output: string = input.replace(pattern, ""); 

console.log(output); // פלט: "hello"
```

במקום ליצור בדיקות מסורבלות לפני המחיקה של תווים מסוימים, אנו פשוט אומרים למחרוזות להחליף את התווים המתאימים לתבנית עם מחרוזת ריקה. זה מבטיח שרק התווים המתאימים יימחקו מן המחרוזת, מה שמאפשר פשטות ויעילות יותר בקוד.

## Deep Dive

מחיקת תווים התואמים לתבנית היא טכניקה נפוצה במגוון שפות תכנות ומטרתה היא לשפר את יעילות הקוד ולהפחית בעיות של ריצה. ב TypeScript, ניתן להשתמש בפונקציה "replace" של מחרוזת כדי למחוק תווים מתאימים לתבנית. כמו בכל תבנית תחבירית אחרת, נעשה שימוש בסמלי ה+ וה* כדי להתאים למספר של תווים נתונים בכל מספר או סוג.

## ראו גם

- [מדריך על מחיקת תווים מתאימים לתבנית ב JavaScript](https://www.w3schools.com/jsref/jsref_replace.asp)
- [מידע על כתיבת קוד נקי ויעיל ב TypeScript](https://devblogs.microsoft.com/typescript/writing-clean-code-with-typescript/)