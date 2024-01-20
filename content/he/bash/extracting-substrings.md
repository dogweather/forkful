---
title:                "חילוץ תת-מחרוזות"
html_title:           "Bash: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?

חילוץ תת-מחרוזות הוא פעולה שבה אנו מחלצים חלק ממחרוזת קיימת. מתכנתים משתמשים בה כדי לאותת, לעצב או לשלוט במידע ממחרוזת.

## איך לעשות:

דוגמאות של קוד שתוך בלוקי קוד ```Bash ... ```.
נקודת התחלה שלנו:
```Bash
original_string="Hello Bash and Hebrew readers"
```
לחלץ תת-מחרוזת מתוך מחרוזת, אנו יכולים להשתמש בסינטקס של Bash:
```Bash
substring=${original_string:6:4}
echo $substring
```
הפלט יהיה:
```Bash
Bash
```
## צלילה עמוקה:

חילוץ מחרוזת היא שיטה שנמצאת בשפות תכנות רבות מאז שנות ה-'70. בלי זאת, אף מתכנת לא היה מצליח ליישם אלגוריתמים מורכבים. ב-Bash, אנו משתמשים באופרטורי המערכת המובנים שלה. לעיתים, עלול להיות אפשרי להשתמש בכלים אחרים מויניקס כמו `awk`, `sed` או `grep`.

## ראה גם: 

- [מדריכים לשליטה במחרוזות ב-Bash](https://tldp.org/LDP/abs/html/string-manipulation.html)