---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?
חיפוש והחלפה של טקסט היא פעולה שבה מחפשים מחרוזת מסוימת ומחליפים אותה במחרוזת אחרת. מתכנתים משתמשים בפעולה זו לקיצור זמן פיתוח, שיפור יעילות, ולטיפול בשגיאות.

## כיצד לביצוע:
אפשר להחליף מחרוזת בקובץ מסוים באמצעות הפקודה `sed`, כך:
```Bash
sed 's/old-text/new-text/g' filename
```
זה יחפש את `"old-text"` בתוך `filename` ויחליף את זה ב `"new-text"`.

## צלילה עמוקה:
הפקודה `sed` הייתה חלק מקובץ ההפקודות של Unix מאז 1974. זו כלי עוצמתי, אך קיימות חלופות מודרניות כמו `perl` ו `awk`.
במקרה של `sed`, החיפוש הוא מבוסס רגקס (ביטויים רגולריים), ולכן אפשר להשתמש בה בצורה גמישה.
המימוש של `sed` עמוד בשכבת ה-Shell של Unix/Linux, הוא אינו מחליף את הקובץ עצמו ללא הכרה בכך אם לא מבקשים ממנו זאת בצורה מפורשת.

## ראה עוד:
1. [חיפוש והחלפה באמצעות Perl](https://www.perl.com/pub/2004/10/13/perl101five.html/)
2. [עוצמת המילה: AWK](https://www.gnu.org/software/gawk/manual/gawk.html/)
3. [חזרה לעתיקות: sed](https://www.gnu.org/software/sed/manual/sed.html/)