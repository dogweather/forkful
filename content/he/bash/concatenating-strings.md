---
title:                "חיבור מחרוזות"
html_title:           "C++: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

# מה ולמה?
השרשור של מחרוזות הוא התהליך של שילוב שני או יותר של רציפים של מאותיות. תכנתים מרבים לבצע שרשור לניצול מירבי של יעילות שפה וזמן ריצה של הקוד.

# איך לעשות:
```Bash
a="היי, זה "
b="ערך של מחרוזת"
c="$a$b"
echo $c
```
הפלט:
```Bash
היי, זה ערך של מחרוזת
```

# הצצה מעמיקה
שרשור מחרוזות הוא חלק הכרחי מערכים מבוססי טקסט ותכנות ב-shell במיוחד. בקודים הישנים, שרשור המחרוזות נעשה באופן ישיר. לפניכן, תכנתים משתמשים בפקודות כמו `cat`, `printf`, או תכנים משלהם. אבל גרסאות חדשות של Bash מאפשרות לנו לשרשר מחרוזות בצורה מאוד יעילה. 

# ראה גם
* [תיעוד ה-RFC ל-Bash](https://tools.ietf.org/html/rfc4254#section-5.2) למי שרוצה לחקור יותר לעומק סוגיות אלו.
* [עמוד הרדמה של Bash](https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html) - גרסה מעודכנת ומפורטת יותר של מדריך בסיסי זה.
* [מדריך מקיף ל- Bash](https://www.tldp.org/LDP/abs/html/) - למי שרוצה ללמוד עוד ולהרחיב את יכולות ה-script שלו.