---
title:    "Fish Shell: מציאת האורך של מחרוזת"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## למה:
חיפוש עוצמת אורך של מחרוזת יכול להיות כלי חשוב בתכנות בשפת פיש. הדבר יכול לעזור לך לקבל מידע חשוב על הנתונים שאתה משתמש בהם, וכן לטפל בבעיות כאשר המחרוזת קצרה מדי או ארוכה מדי.

## איך לעשות:
כדי למצוא את אורך המחרוזת בפיש, יש להשתמש בפקודת `string length`. נסו את הקוד הבא כדי לראות איך הפקודה עובדת:
```fish
string length "hello"
```
פלט:
```
5
```

ניתן גם לשלב את פקודת `string length` עם פקודת `echo` כדי להדפיס את הערך יחד עם הטקסט. לדוגמה:
```fish
echo "אורך המחרוזת שלי הוא" (string length "fish shell")
```
פלט:
```
אורך המחרוזת שלי הוא 10
```

## חקירה מעמיקה:
תוכלו למצוא מידע נוסף על פקודת `string length` בתיעוד של פיש. ניתן לגשת לתיעוד על ידי הפקודה `help string length` או לכתוב `man sting`. בנוסף, פקודת `string length` מוסברת בעומק בבלוג "תכנות בפיש" של מסעדת הקוד ה- 10.

## ראו גם:
- [תיעוד על פקודת `string length`](https://fishshell.com/docs/current/cmds/string-length.html)
- [בלוג "תכנות בפיש" - מחרוזת](https://blog.fishshell.com/194-fish-string.html)
- [מדריך תחביר ופקודות בפיש](https://fishshell.com/docs/current/index.html)