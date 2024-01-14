---
title:                "TypeScript: ייצור מספרים אקראיים"
simple_title:         "ייצור מספרים אקראיים"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

סוכנים שלוש הקלטות גיאומטריות וחד גרעיניות ליטרליות או נקודת מקור . זה מאפשר למערכת ליצור מספרים אקראיים בכמה צורות שונות ולהשתמש בהם ליישומים מגוונים כמו משחקים, סטטיסטיקות וכו'. "המחשבה מאחורי ליצור מספרים אקראיים היא ליצור סיבובות חופשיות ולקבל את התוצאה הרנדומלית. זה מאפשר היצירה של מספרים כמעט בלתי אפיינים ותואמים לאינפורמציה גולמית שלהם."

# מדוע

היצירת מספרים אקראיים יכולה להיות מועילה ביישומים רבים למשל משחקים, סטטיסטיקות, אבטחה ועוד. השתמש באלגוריתם זה כדי ליצור סיבובות חופשיות ותואמות לאינפורמציה גולמית של המספרים.

# איך לעשות

קוד הפתיחה הבא מדגים איך ליצור מספר אקראי בין 1 ל 10 ולהדפיס אותו בלוג הקונסול של הדפדפן:

```
TypeScript
// יצירת מספר אקראי בין 1 ל 10
let randomNumber = Math.floor(Math.random() * 10) + 1;
// הדפסת המספר האקראי
console.log(`המספר האקראי הוא: ${randomNumber}`);
```

פלט של הלוג יהיה משהו דומה לזה: "המספר האקראי הוא: 6"

# מקורות נוספים

ישנם אלגוריתמים רבים שניתן להשתמש בהם כדי ליצור מספרים אקראיים. למידע נוסף על יצירת מספרים אקראיים, ניתן להציץ בכתבי העת הבאים:

- [Generating Random Numbers in TypeScript](https://www.tutorialspoint.com/generating-random-numbers-in-typescript)
- [The Math.random() Function](https://www.w3schools.com/js/js_random.asp)
- [Random Number Generation in TypeScript](https://www.geeksforgeeks.org/random-number-generation-in-typescript/)
- [Generating Random Numbers in TypeScript using the Random Package](https://www.