---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
מציאת אורך המחרוזת היא פעולה שבה מחשבים את מספר התווים המרכיבים מחרוזת מסוימת. תכנתים לרוב משתמשים בזה לבדוק בתוקף של קלט, ללולאות או שחזורים ותיקונים של מחרוזות.

## איך לעשות:
בצורה בסיסית, ניתן למצוא את אורך המחרוזת ב-TypeScript בעזרת המאפיין length של המחרוזת:
```TypeScript
let myString: string = "תכנות ב-TypeScript";
console.log(myString.length);  // ידפיס לכם 18
```

אם המחרוזת שלכם מכילה תווים יוניקוד, אז האורך עשוי לא להיות מה שאתם מצפים:
```TypeScript
let myUnicodeString: string = "מחרוזת עם אימוג'י 😄";
console.log(myUnicodeString.length);  // ידפיס לכם 18, לא 17
```
## צלילה עמוקה
הפקודה `.length` התפתחה במטרה לספק דרך יעילה ונוחה למציאת אורך של מחרוזת. זכורים, למרות זאת, שהספירה של `.length` היא 1-מבוססת, אז את תווים האוניקוד מחושבים כ-2 תווים. ייתכן אי-התאמה כאשר מתמודדים עם תווים יוניקוד. 

כדי להתמודד עם בעיות שכאלה, לנו יש את מתודת `Array.from()`, המאפשרת ספירת תווים בצורה מדויקת יותר:
```TypeScript
let accurateLength: number = Array.from(myUnicodeString).length; // יחזיר 17
```
## ראו גם
- [MDN Web Docs: String.length](https://developer.mozilla.org/he/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Stack Overflow: How to get a character's length in JS](https://stackoverflow.com/questions/54369504/javascript-how-to-get-character-length)