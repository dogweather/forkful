---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 
המרת תאריך למחרוזת ("string") היא פשוטה - המרת בלוק מידע שמייצג תאריך לסדר של תווים. נעשה את זה כדי להקל את הטיפול בתאריך בתוכניות שמטפלות בטקסט, כמו דיבוג, דיווח או שמירה במסד נתונים.

## איך לעשות:

באמצעות פקודת `date` ב-Bash, אתה יכול להמיר תאריכים למחרוזות.

```Bash
#!/bin/bash
# עכשיו
now=$(date)
echo $now

# בפורמט שונה
formatted=$(date +"%Y-%m-%d")
echo $formatted
```

## כניסה לעמקים 

Bash הינה מעטפת שורת פקודה ולקחה להן הרבה זמן להתפתח. במקרה שלנו, `date` הוא תכנית UNIX ישנה שמגיעה עם הפקודות הבסיסיות של המערכת. ישנם אלטרנטיבות ל-`date` אך הן בדרך כלל קשורות לתכנות כבד יותר כמו Python או Java.

אחת הפרטים המעניינים של ההמרה היא שהמחרוזת שמיוצרת מתאריך בהכרח מותאמת לשפה של המערכת. אם תחליף את השפה, גם הסדר של יום-חודש-שנה ישתנה.

## ראו גם
+ https://he.wikipedia.org/wiki/מעטפת_פקודה
+ https://www.tutorialspoint.com/unix/unix-date-time.htm
+ https://unix.stackexchange.com/questions/164826/bash-date-command-not-found