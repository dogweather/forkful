---
title:                "שליפת תת-מחרוזות"
html_title:           "Bash: שליפת תת-מחרוזות"
simple_title:         "שליפת תת-מחרוזות"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?

הפעלת משפטים חלקיים היא כלי שבה תחתונה, מאפשר לנו להוציא חלק מתוך מחרוזת כדי להשתמש בו לצורך פעולות ספציפיות. תוכנתנים נוהגים להשתמש בשיטה זו כדי להפוך את קוד התכנית שלהם לנקי ויעיל, וכן להפעיל משימות כגון איתור מידע בקבצים מרובים או עיבוד נתונים שבהם רק חלק מהמידע חשוב.

## איך לבצע:

כדי לפעול על מחרוזת בתוך קוד באש, יש להשתמש בפקודה ```echo``` עם האופציה ```${string:start:end}```, כאשר "string" הוא המחרוזת המקורית, "start" הוא האינדקס של התו הראשון של החלק הרצוי, ו-"end" הוא האינדקס של התו האחרון של החלק הרצוי. לדוגמה:

```Bash
user_input="Hello World"
echo ${user_input:0:5}
```

כתוצאה מהפקודה הזו, המחרוזת "Hello" תוחלף במקור המחרוזת "Hello World". ניתן גם להשתמש במספר שליליים כאינדקס, כך שהחלק שיוחסה יתחיל מהסוף של המחרוזת.

## מעמקים:

לאחר שנחבר גם כינויות, בשם "String slicing" או "Substring extraction", השימוש באמצעות מחרוזת נהפך לפופולרי יותר בסביבת תכנות נעים. כיוון שזהו חלק בסיסי מהשפות השונות, כנראה כי בקרוב יכלל את מעמספים כבשפת שליטה בחדר הפעולה של תכנית. ישנם בנקודות יתירים גם השתמשתי רק אחת האפשרויות, המוסיף גם נבדק, ואילו באמצעות טכניקות אחרות ניתן לשנות את ה־18, כך שתכנית תגיע לעץ של כל המחרוזות.

## ראו גם:

- [שימוש אברההאם](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html#Bash-Variables)
- [מיפוי אופציות בפקודת Echo](https://www.computerworld.com/article/2836832/how-to-create-mapping-in-bash.html)
- [כתיבת אדונט אבסולות בבאש](https://counter_reforms.blogspot.com/technology-tweet.html)