---
title:                "חיפוש והחלפת טקסטים"
html_title:           "TypeScript: חיפוש והחלפת טקסטים"
simple_title:         "חיפוש והחלפת טקסטים"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## למה

למה לדאוג לחיפוש והחלפת טקסט בתוך תוכנית פיתוח ב-TypeScript?

מעבר לטכנולוגיות אחרות, טכנולוגיית ה-TypeScript מציעה ספריות עוצמתיות לפעולות החיפוש והחלפה של הטקסט שאנחנו מתאמצים לטפל בתוכנית שלנו. היכולת לבצע מניפולציות על טקסט היא מיוחדת ל-TypeScript ויוכלו להוביל לנו לקוד יותר יעיל ומתואם.

## איך לעשות

הנה כמה דוגמאות של החיפוש והחלפה של טקסט ב-TypeScript:

```TypeScript
// כאן אנו חיפוש ומחליפים את המילה "ברוך" עם המילה "שלום" במשפט ספציפי
const sentence: string = "ברוך הבא לעולם הנגיש של TypeScript";
const newSentence: string = sentence.replace("ברוך", "שלום");
console.log(newSentence);
// output: שלום הבא לעולם הנגיש של TypeScript
```

```TypeScript
// ניתן להשתמש גם בפונקציה ע"י שימוש בפרמטרים ובנסיגים
const greeting: string = "Hello, {name}";
const replaceName = (name: string): string => {
  return greeting.replace("{name}", name);
}
console.log(replaceName("John"));
// output: Hello, John
```

```TypeScript
// אם אתה רוצה להחליף יותר מפעם אחת, תוכל להשתמש בפקודת רגולר אקספרשנס
const sentence: string = "זה היה הלוויה המספקת ביותר של כולם";
const newSentence: string = sentence.replace(/ויה/i, "וויה");
console.log(newSentence);
// output: זה היה הלווייה המספקת ביותר של כולם
```

## עיון מעמיק

בנוסף לפונקציונליות הסטנדרטית של החיפוש והחלפה ב-TypeScript, ישנם גם כמה מאפיינים מתקדמים שניתן להשתמש בהם כדי להתאים את המילון ולבצע מניפולציות נוספות על הטקסט. לדוגמא, ניתן להשתמש בפרמטר "flags" כדי להגדיר התאמות נוספות מ