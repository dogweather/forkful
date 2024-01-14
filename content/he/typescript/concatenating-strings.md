---
title:                "TypeScript: צירוף מחרוזות"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה

משולש משתנים? התאמץ ...

## איך לעשות

```TypeScript
let hello: string = "שלום";
let name: string = "עולם";
let greeting: string = hello + name;

console.log(greeting);
```

פלט:
```TypeScript
שלוםעולם
```

## חקירה מעמיקה

לשרשרת היא פשוט כדי לחבר מחרוזות יחד. מצד שני, ייתכן שתתקלו במצבים שבהם תצטרכו לחבר מחרוזות עם משתנים או לחבר מחרוזות עם תווים מיוחדים. בכדי לטפל בכל מצב, ניתן להשתמש בתווים פיענוח כדי לצייר את המחרוזת המבוקשת.

## ראה גם

- [תיעוד TypeScript רשמי על התאמות שרשרת](https://www.typescriptlang.org/docs/handbook/variable-declarations.html#string-concatenation)
- [מדריך להשתמש במשבצת-לשרשורת ב-TypeScript](https://mariusschulz.com/blog/using-template-strings-in-typescript)
- [הפרוטוקולים והמדריכים לשרשרת מחרוזות ב-TypeScript](https://ultimatecourses.com/blog/understanding-typescript-template-strings)