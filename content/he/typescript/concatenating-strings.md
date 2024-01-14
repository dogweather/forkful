---
title:                "TypeScript: לחבר מחרוזות"
simple_title:         "לחבר מחרוזות"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה

תהליך הזכרון של סדרתי תווים נקרא תהליך הזכרון הטנפני. הוא משמש להוספת תווים למחרוזת קיימת או ליצירת מחרוזות חדשות מסוימות. זה מאפשר לסנכרן מחרוזות שונות יחד וליצור מחרוזות דינמיות בהתאם לצורך.

## איך לעשות

```TypeScript
let name: string = "דנה";
let greeting: string = "שלום";
let message: string = greeting + " " + name + ", אני מאוד שמחה להכיר אותך!";
console.log(message);
```

פלט:
```
שלום דנה, אני מאוד שמחה להכיר אותך!
```

בדוגמה זו, אנו משתמשים בתהליך זכרון טנפני כדי לחבר מחרוזות. תחילה, אנו מכריחים את ערך המשתנה להיות מחרוזת. לאחר מכן, באמצעות סימן הפלוס (+) אנו מצרפים את המחרוזות השונות יחד כדי ליצור את המחרוזת החדשה המרוכזת.

## צליינה עמוקה

תהליך הזכרון הטנפני יכול להיות מאוד שימושי בתכנות טיפוסי. כאשר אנחנו משתמשים בסימן פלוס (+) כדי לחבר מחרוזות, אנו משתמשים בתהליך הזכרון הטנפני בפנים, מאחורי הקלעים. זה מאפשר לנו להיות יצירתיים ויעילים בכתיבת קוד וליצור מחרוזות דינמיות באופן יעיל.

## ראו גם

- [MDN: תהליך הזכרון הטנפני](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Addition_assignment)
- [קודיפנדיה: תהליך הזכרון הטנפני](https://www.codecademy.com/learn/introduction-to-javascript/modules/learn-javascript-introduction/cheatsheet)