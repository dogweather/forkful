---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "TypeScript: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## למה

למה להתעסק בהמרת מחרוזת לתווי אותיות קטנות?

המרת מחרוזות לתווי אותיות קטנות היא פעולה נחשבת קטנה אך חשובה בתכנות. היא מאפשרת לנו לפענח מחרוזת לטקסט נוח יותר לעיבוד ולהשוואה. כמו כן, כתיבת קוד עם תווי אותיות קטנות עוזרת לשמור על קוד נקי וקריא, והיא מאפשרת לנו להימנע מבעיות הקשורות לאיחסון והשוואה של מחרוזות.

## איך לעשות זאת

### עם הפונקציה toLowerCase()

הפונקציה הפשוטה ביותר להמרת מחרוזת לתווי אותיות קטנות ב-TypeScript היא להשתמש בפונקציה הקיימת toLowerCase(). ניתן להשתמש בה על ידי הזנת המחרוזת שברצוננו להמיר בתוך סוגריים מרובעים כזה:

```typescript
let string = "HeLlo WoRld";
let lowerCaseString = string.toLowerCase();
console.log(lowerCaseString); // "hello world"
```

### עם ספריית Lodash

ל-Swift יש ספריית נוספת שנקראת Lodash המכילה פונקציות שימושיות לניהול טקסט. ניתן להשתמש בפונקציות מסוימות מתוך הספרייה הזו כדי להמיר מחרוזות לתווי אותיות קטנות. לדוגמה, הפונקציה toLower תפעול בדיוק כמו הפונקציה toLowerCase שראינו מקודם:

```typescript
import { toLower } from "lodash";
let string = "HeLlo WoRld";
let lowerCaseString = toLower(string);
console.log(lowerCaseString); // "hello world"
```

## מעמקים

ישנם מספר דרכים לממש את התוכנית לקיצור מחרוזות לתווי אותיות קטנות. אחת הדרכים היא להשתמש בלולאה for ולעבור על כל תו במחרוזת ולהמיר אותו לתו קטן בעצמו. פתרון זה בדיוק מה שספריית Lodash עושה מאחורי ה