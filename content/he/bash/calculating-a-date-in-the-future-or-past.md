---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "Bash: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר הוא יכולת למצוא תאריך ממקום שונה בציר הזמן. זה מועיל לתכנתים לטיפול בדרישות לתאריכים משתנים, כמו מדד זמן, או כדי להגדיר תקופות זמן ספציפיות.

## איך בדיוק?
להלן דוגמאות לתכנות ב-Bash שמדגימות את החישוב של תאריך בעתיד או בעבר:

```Bash
# חישוב תאריך 3 ימים לפני היום הנוכחי
date -d "3 days ago"

# חישוב תאריך 5 ימים לאחר מכן
date -d "5 days"
```

## צלילה עמוקה
התכנית `date` של UNIX הייתה הדרך המקורית לחישוב תאריכים בעבר או בעתיד. אך מאז, ישנם דרכים חדשות לעשות את זה, כמו `strtotime()` ב-PHP או  השימוש ב- libraries של ג'אווה. מעבר לזה, ההבנה של איך bash מחשב תאריכים מספקת תבניות לפתרונות בתחום זה.

## ראה גם
- מדריך לאיך להשתמש בפקודת date ב-Bash: https://www.cyberciti.biz/faq/unix-linux-getting-current-date-in-bash-ksh-shell-script/
- הבנה מעמיקה יותר של date command ב UNIX: https://www.geeksforgeeks.org/date-command-linux-unix-examples/
- למידה נוספת אודות Bash: https://www.learnshell.org/