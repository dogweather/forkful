---
title:                "TypeScript: חיפוש והחלפת טקסט"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# למה

לחיפוש והחלפת טקסט הוא כלי חשוב בתכנות ומאפשר לכתוב קוד קריא ויעיל. הוא מאפשר למצוא ולהחליף יחד חלקים שונים בקוד שבתכנית נדרשת הישרדות.

## כיצד לעשות

הנה כמה דוגמאות של חיפוש והחלפת טקסט בתכנון TypeScript עם פלט דוגמה:

```TypeScript
// קוד דוגמה עם אתחול מחרוזת
let myString = "שלום עולם";
console.log(myString.replace("עולם", "לילה"));

// פלט: "שלום לילה"
```

```TypeScript
// צור פונקציה קצרה לחיפוש והחלפת מחרוזת
function replaceInString(searchString: string, replaceString: string, targetString: string) {
  return targetString.replace(searchString, replaceString);
}

console.log(replaceInString("אדוני", "גבר", "האדון הוא אדון נפלא"));

// פלט: "הגבר הוא גבר נפלא"
```

## חקירה עמוקה

חיפוש והחלפת טקסט הוא פעולה חשובה בתכנון כי זה מאפשר לכתוב קוד גמיש יותר ולתפוס מקרים מיוחדים כשהם מתכונסים. בכל פונקציה של TypeScript שלחיפוש והחלפתה של מחרוזת תוכלו למצוא אופציות להשתמש בפלט כאשר המחרוזת מתאימה לטקסט שהועבר.

## ראה גם

[התחלת תכנות TypeScript בקלות] (https://hebrew.codementor.io/@dannykoz/hebrew-typescript-tutorial-iyxkw2qu4)
[Top 10 כלים לתכנות TypeScript] (https://blog.bitsrc.io/10-typescript-tools-to-level-up-your-app-development-ecada385fed3)