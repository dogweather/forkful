---
title:                "מציאת אורך מחרוזת"
aliases:
- /he/javascript/finding-the-length-of-a-string/
date:                  2024-01-20T17:48:12.191560-07:00
model:                 gpt-4-1106-preview
simple_title:         "מציאת אורך מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
האורך של מחרוזת מאפשר לדעת כמה תווים יש בה. תכניתנים צריכים את המידע הזה לבדיקות ולעיבוד של טקסט.

## איך עושים את זה:
```Javascript
let greeting = 'שלום עולם!';
console.log(greeting.length); // 11
```
קטע הקוד למעלה מראה את אורך המחרוזת של 'שלום עולם!', שהיא 11.

## צלילה לעומק
ב-JavaScript, האורך של מחרוזת נמצא בקלות באמצעות המאפיין `length`. מאז היווצרות השפה, זוהי דרך סטנדרטית ויעילה לקבלת מידע זה.

אלטרנטיבות לא ממש קיימות כי ה`length` פשוט עובד ומהיר. אבל, ישנם מקרים שבהם ספירת התווים צריכה לדלג על תווים מיוחדים או להתחשב בתווים כפולים (לדוגמה אם משתמשים בתווים מUnicode).

דיטיילים טכניים: בשפות תכנות אחרות, ייתכן שתצטרך לעבור על כל המחרוזת ולספור תווים כדי לקבל את האורך. ב-JavaScript, לאובייקט המחרוזת יש את המאפיין `length` שמחזיק את המידע הזה מראש, מה שהופך את הפעולה למאוד פשוטה ומהירה.

## ראה גם
- תיעוד על אובייקטים מסוג String ב-JavaScript: [MDN String documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- קורס בסיסי על JavaScript, הכולל שימוש במחרוזות: [Codecademy JavaScript Course](https://www.codecademy.com/learn/introduction-to-javascript)
