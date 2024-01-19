---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?
מדפיסים פלט ניפוי במטרה לבדוק ולזהות בעיות בתוך קוד. זה עוזר למתכנתים לאתר באופן מהיר את מקום השגיאה בקוד.

## איך לעשות:
ניתן להדפיס פלט ניפוי ב-Bash באמצעות הפקודה `echo`, כשהמחרוזת לאחר `echo` מהווה את הפלט שמיועד להוצג.

```Bash
#!/bin/bash 

# בדיקה של פונקציה פשוטה
function check() {
  echo "נכנסו לפונקציה check"
}

# קריאה לפונקציה
check
```
הפלט של קוד זה יהיה "נכנסו לפונקציה check".

## צלילה עמוקה
ההדפסה של פלט ניפוי היא שיטה קלאסית שמשמשת מתכנתים מאז שנוצרה. בתוך גרסאות רבות של שפות תכנות, ניתן להשתמש בשיטות חלופיות כמו `printf` ב-C או `println` ב-Java. 

השימוש ב `echo` להדפסת פלט ניפוי הוא גם טכניקה שמחזיקה יתרונות וחסרונות. היתרון המרכזי הוא שהתנהלות המערכת ניתן לזהות יותר בקלות בעזרת הפלט הנמוך במהלך הפעילות. החסרון המרכזי הוא שהפלט ניפוי שיודפס לא מצליח לתאר את המצב של כל משתנה בתוך התוכנית.

## ראה גם
ישנן כמה מקורות המתאימים למי שרוצה להעמיק:
- ספר Bash Guide for Beginners (מדריך Bash למתחילים): https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_02_03.html
- Advanced Bash-Scripting Guide (מדריך מתקדם לכתיבת סקריפטים ב-Bash): https://tldp.org/LDP/abs/html/io-redirection.html
- מאמר על Debugging in Bash (ניפוי ב-Bash): https://www.linuxjournal.com/content/debugging-bash-scripts