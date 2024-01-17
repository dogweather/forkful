---
title:                "שימוש בביטויים רגילים"
html_title:           "TypeScript: שימוש בביטויים רגילים"
simple_title:         "שימוש בביטויים רגילים"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?
שימוש בביטויים רגולריים הוא כלי חזק שנמצא בכלי הכתיבה של תכנותנים. זה מאפשר לנו לחפש ולהתאים מדויק טקסט באמצעות דפוס מתאים. תכנתנים משתמשים בו בכדי לחפש ולעבד קלט טקסט מגוון ולקבל תוצאות מדויקות יותר.

## כיצד ל:
הליכי דוגמא ופלט נכתבים בתוך ספרות "TypeScript ... ". כאן נדגים איך לחפש מילים שונות בטקסט ולשנות את התוצאה הסופית:

```
TypeScript

// חפש כל מילה שמתחילה עם אות "S"
let regex = /S\w*/;
let str = "Search is a powerful tool for programmers";
console.log(str.match(regex));
// Output: ["Search"]

// החלף את המילה "powerful" עם "useful"
let newString = str.replace("powerful", "useful");
console.log(newString);
// Output: "Search is a useful tool for programmers"

```

## עמוק בלבד:
* היסטוריה: ביטויים רגולריים נמצאים אז מאוד מאוחר וניצלו בכלי הכתיבה של פורמטים שונים עד שלסוף באננו לתכנות.
* אלטרנטיבות: תכנתנים יכולים גם להשתמש בטכניקות נוספות עבור חיפוש ועיבוד טקסט, כמו פקודות כמו ```indexOf()``` and ```substring()```, אבל הם לא יכולים לספק את אותה רמת תמיכה כמו ביטויים רגולריים.
* פרטים למימוש: תכנתנים מקריאים מתיר האמרגוזים נוספים של יישומים ביטויים רגולאריים. כאילו דוגמאות נוספות, כיצד לשפר קוד חיפוש ודפוס בביטויים רגולריים עי רשמות ושאר פירטימו.

## ראה גם:
להלן כמה קישורים למידע נוסף על ביטויים רגולריים בתוכנה TypeScript:
* [תיעוד ראמי של ביטויים רגולריים ב-TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
* [הכרחים ודוגמאות ביטויים רגולאריים של תיעוד ב-TypeScript](https://www.typescriptlang.org/docs/handbook/regex.html)
* [מדריך תברואתית ביטויים רגולריים ב-TypeScript נכתב ב־CodeTuts+](https://code.tutsplus.com/tutorials/8-regular-expressions-you-should-know-a-tutorial-by-the-experts--net-6149)