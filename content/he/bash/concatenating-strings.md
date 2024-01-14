---
title:                "Bash: חיבור מחרוזות"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה 
מדוע עוסקים בחיבור מחרוזות? לדוגמה, כדי ליצור מחרוזות ארוכות יותר על יסוד מחרוזות קצרות, להדפיס מידע מרוכז בכתבות או לייצר בתכניות שורות תוכן.

## כיצד לעשות את זה
 לחיבור מחרוזות. השתמש בפקודה "echo" ואת הסימן השלישי הפשוט ">" לטיוב. כדי לחבר מחרוזות נוספות, הוסף את הסימן השלישי הכפול ">>" לטיוב כדי להוסיף את המחרוזת אל מחרוזת הטיוב הראשונית. לדוגמה:

```Bash
echo "שלום" > hello.txt
echo ", עולם" >> hello.txt
cat hello.txt
```

 כתוצאה מקוד זה, הרצת התוכנית תגדיר מחרוזת "שלום, עולם" עם נקודה בסופה.

## חקירה מעמיקה
 כאשר משתמשים בפקודת "echo", בפועל מופעלת כדי להדפיס גם רווחים וכרטיסיות. יש לציין שלא כל הפקות של לינוקס כוללות את הפקודה "echo", על כן חשוב לוודא את התמיכה בפקודה בגרסת המערכת על ידי הרצת "echo --help" בכדי לקבל מידע פרטי. 

## ראה גם
- [שימוש בפקודת "echo" כדי להדפיס גירסאות של קבצים](https://linuxcommand.org/lc3_man_pages/echoh.html)
- [מדען המחשב על פקודות טרמינל](https://www.codecademy.com/learn/learn-the-command-line/modules/bash-scripting)
- [למד לכתוב תוכניות Bash איכותיות](https://thesephist.com/posts/writing-bash-scripts/)