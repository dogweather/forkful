---
title:                "Javascript: כתיבת מחרוזת באותיות רישיות"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מדוע

למה להשקיע זמן ומאמץ כדי לשנות את האות הראשונה של מחרוזת ולהפוך אותה לאות גדולה?

## כיצד לעשות זאת

אם תרצה לשנות את האות הראשונה של מחרוזת לאות גדולה בצורה מהירה ויעילה בקוד שלך, ישנם כמה דרכים שאתה יכול לעשות זאת ב-Javascript:

```Javascript
// דרך 1: שימוש בפונקציית toUpperCase():
let str = "hello world";
console.log(str[0].toUpperCase() + str.slice(1)); // הפלט יהיה "Hello world"

// דרך 2: שימוש במתודת replace():
let str = "good morning";
console.log(str.replace(str[0], str[0].toUpperCase())); // הפלט יהיה"Good morning"
```

## התעמולה

למרבה המזל, טכנולוגיות כמו Javascript עובדות עם פעולות על מחרוזות בצורה יעילה, ולכן העלאת האות הראשונה של מחרוזת לאות גדולה הופכת לתופעה הרבה פחות נדירה מאשר בקודים של טכנולוגיות אחרות.

כל פעם שיש לנו מחרוזת שאנחנו רוצים להציג למשתמש בתוך תוכנית כלשהי, הסיבה שבגללה אנחנו משתכנים כמפתחים היא בגלל שאנחנו רוצים להציג שרשרת מילים המתאימה לפורמט שבו אנחנו מוציאים את המידע שלנו. לדוגמה, נניח שנרצה להציג את המחרוזת "ברוך הבא" על גבי המסך כשהמשתמש נכנס לאתר. את המחרוזת הזו עלינו לכתוב על גבי המסך בפורמט מתאים עם האות הראשונה מופיעה כאות גדולה, ולכן אנו מפעילים פונקציה המביאה את המחרוזת הזו כמו: "ברוך הבא".toUpperCase();

## ראה גם

- [מידע נוסף על הפונקציות toUpperCase() ו-replace() של Javascript](https://www.w3schools.com/jsref/jsref_touppercase.asp)
- [פונקציות לעיב