---
title:                "אינטרפולציה של מחרוזת"
html_title:           "Arduino: אינטרפולציה של מחרוזת"
simple_title:         "אינטרפולציה של מחרוזת"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

# תרגול מחרוזת ב-TypeScript: המלאך בפרטים הקטנים

## מה זה ולמה?

תרגול מחרוזת הוא דרך להכניס משתנים או ביטויים ישירות לתוך מחרוזת. המתכנתים משתמשים בזה לצורך פשטות של קוד ולהפחתת הסיכוי לשגיאות.

## איך לעשות את זה:

לעיבוד מחרוזת ב-TypeScript נשתמש ב-back-ticks ובמשתנים בסוגריים משולשים. כמו בדוגמה הבאה:

```TypeScript
let name = 'Dani';
console.log(`שלום, ${name}!`); // פלט: "שלום, Dani!"
```

## צלילה לעומק:

הרעיון של תרגול מחרוזות הוא לא חדש וכבר היה נמשך בשפות כמו Perl ו-Ruby. ב-TypeScript (וב-JavaScript עצמה), הוא משמש כדרך מעולה למניעת שגיאות שגיאה במחרוזת.

דרך חלופית לתרגול מחרוזת ב-TypeScript היא באמצעות שימוש בשיטת `+` או 'concat'. אך שיטות אלה הן פחות קריאות ויותר נוטות לשגיאות.

ראשית, בTypeScript, המחרוזת מתרגלת בזמן הרצה ולא כחלק משלב ההדפסה, מה שמאפשר ביטויים מורכבים ופונקציות שיכולות לשגר ערכים למחרוזת.

## ראה גם:

- [TypeScript שיעורים ומדריכים](https://www.typescriptlang.org/docs/)
- [המסמך המקורי של תרגול מחרוזות](https://developer.mozilla.org/he/docs/Web/JavaScript/Reference/template_strings)
- [השוואה בין שיטות תרגול](https://www.digitalocean.com/community/tutorials/how-to-use-string-literals-in-typescript)