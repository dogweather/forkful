---
title:                "TypeScript: מציאת אורך מחרוזת"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## למה?

במחשבים ותכנות, לעיתים קרובות נתקלים בצורך לקבוע את האורך של מחרוזת (string). זה עשוי לקרות בגלל צורך לוודא שהמחרוזת לא תוכיח להיות ארוכה מדי למטרות ספציפיות, או בגלל צורך לבצע מניפולציות על המחרוזת. במאמר זה נלמד איך למצוא את האורך של מחרוזת באמצעות טיפול מתאים בקוד TypeScript.

## איך לבצע זאת?

בשפת TypeScript קיימות כמה דרכים למצוא את האורך של מחרוזת. בכתיבת קוד טובה וקריאה, כדאי להשתמש בשיטת `length` הקיימת כבר במחרוזת עצמה. כך, לדוגמה, נרצה להציג את אורך המחרוזת "Hello World" בעזרת הפקודה `console.log`:

```TypeScript
let str: string = "Hello World";
console.log(str.length);
```

יצא בטרמינל:
```
11
```

ניתן גם להשתמש בפונקציה `length` של עצמה בשביל לקבל את אורך המחרוזת. נציב את המחרוזת "Goodbye" במשתנה חדש ונשתמש בפעולת `length` כדי להדפיס את אורךה:

```TypeScript
let message: string = "Goodbye";
console.log(message.length);
```

הפלט בטרמינל יראה כך:

```
7
```

יש גם אפשרות להתייחס לאורך המחרוזת באמצעות פונקציה מותאמת אישית. במקרה זה, נחלק את המחרוזת לתווים יחידים ונספור אותם בעזרת לולאת `for` ומשתנה מספרי `counter`:

```TypeScript
function findLength(str: string): number {
    let counter: number = 0;
    for (let i = 0; i < str.length; i++) {
        counter++;
    }
    return counter;
}

let text: string = "JavaScript is fun";
console.log(findLength(text));
```

יצא בטרמינל:

```
18
```

## חקירה עמוקה

כמו שראינו, דרכי האתרוג כדי לקבל את האורך של מחרוזת לא מורשה הן רבות ושונות. בקוד שכתבנו מעל, ש