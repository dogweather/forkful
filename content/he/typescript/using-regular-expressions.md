---
title:                "שימוש בביטויים רגולריים"
date:                  2024-01-19
html_title:           "Bash: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"

category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
מבטאים רגולריים זה כלי לחיפוש וחלפת טקסט לפי תבניות. מתכנתים משתמשים בהם כדי לעבד נתונים ולוודא אמיתות קלט מהר ובקלות.

## How to: (איך לעשות:)
```TypeScript
// חיפושים פשוטים
const text: string = 'מצא את כל המילים שמתחילות באות "ה"';
const regex: RegExp = /ה\w+/g;
const found: string[] = text.match(regex);
console.log(found); // ['המילים', 'המתחילות']

// חלפת טקסט
const newText: string = text.replace(regex, "היי");
console.log(newText); // "מצא את כל היי שהיי באות "ה"

// ולידציית קלט
const email: string = 'example@mail.com';
const emailRegex: RegExp = /^\S+@\S+\.\S+$/;
console.log(emailRegex.test(email)); // true
```

## Deep Dive (לעומק העניינים)
ב-1960, מתמטיקאי בשם סטיבן קליני תיאר תחביר שנקרא כעת "מבטאים רגולריים". בתכנות מודרני, קיימות אלטרנטיבות כמו פרסרים סינטקטיים לתרחישים מורכבים, אבל מבטאים רגולריים נותרים יעילים לביצועי רוב המשימות הקשורות לטקסט. רוב שפות התכנות כוללות תמיכה מובנית במבטאים רגולריים, וב-TypeScript תמיכה זו מגיעה דרך המחלקה RegExp של JavaScript.

## See Also (ראה גם)
- [MDN Regular Expressions Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions) - מדריך על מבטאים רגולריים ב-JavaScript, שקשורים ישירות ל-TypeScript.
- [RegExr](https://regexr.com/) - כלי אינטרנטי לבדיקת ולמידת מבטאים רגולריים.
- [RegExp TypeScript Documentation](https://www.typescriptlang.org/docs/handbook/2/objects.html#regexps) - תיעוד רשמי על שימוש ב-RegExp ב-TypeScript.
