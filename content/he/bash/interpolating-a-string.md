---
title:                "אינטרפולציה של מחרוזת"
html_title:           "Arduino: אינטרפולציה של מחרוזת"
simple_title:         "אינטרפולציה של מחרוזת"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
החילוף של מחרוזות ב-Bash הוא שיטה להוספת משתנים או ביטויים לתוך מחרוזת. זה נותן למתכנתים גמישות ליצור מחרוזות דינמיות ולאוטמט את התהליכים.

## איך לעשות זאת:
שימוש בחילוף מחרוזות פשוט וישיר ב-Bash:

```Bash
name="Yossi"
echo "שלום, $name"
```
ערכו של $name מתוך המחרוזת עצמה מתחלף עם הערך של המשתנה:

```Bash
שלום, Yossi
```

## צלילה עמוקה
חילוף מחרוזות מגיע מהימים הקדמוניים של עורך הטקסט של Unix. בהקשרים מסוימים, ייתכן שתרצה להשתמש במנגנונים מתקדמים יותר ממחרוזת הסטנדרטית, כמו `printf` או `sprintf`. על פי ברירת מחדל, Bash משתמש בערך "עגלויה" (`"\0"`), אבל ץמיה במריח םהתאם ערך של משתנה.

## ראה גם
למידת Bash: [https://he.learnbash.co.il/](https://he.learnbash.co.il/)
מדריך Bash: [https://www.tutorialspoint.com/he/unix/unix-string-manipulation.htm](https://www.tutorialspoint.com/he/unix/unix-string-manipulation.htm)