---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
השוואה בין שני תאריכים היא אבחון של מי מהם קודם, מאוחר יותר, או ששניהם שווים. מפתחים עושים זאת למשל כדי לבצע כמה פעולות בהתאם לתוצאה, כמו הזמנת פעולות מתוזמנות או חישוב התקופה שחלפה מאז תאריך כלשהו.

## כיצד:
אנחנו יכולים להשוות בין שני תאריכים ב- Bash באמצעות הפקודה `date`. להלן דוגמה:

```Bash
d1=$(date -d "2022-05-01" +%s)
d2=$(date -d "2022-06-01" +%s)

if (( d1 > d2 )); then
    echo "Date1 is later than Date2"
elif (( d1 < d2 )); then
    echo "Date1 is earlier than Date2"
else
    echo "Both dates are equal"
fi
```

אם התאריך הראשון מאוחר יותר מהשני, הקוד יחזיר: "Date1 is later than Date2", ואם הפוך הוא המצב, יחזור: "Date1 is earlier than Date2", ואם שני התאריכים שווים יחזור: "Both dates are equal".

## צלילה עמוקה:
שימוש ב- Bash להשוואת תאריכים הוא רק אחת מהאפשרויות. פקודת `date` נכתבה לראשונה על ידי Ken Thompson בשפת התכנות Cאשר עצמה נכתבה בשנת 1972, כאשר ה- Unix התפתח. קיימות גם גירסאות נוספות של `date`, כמו `GNU date` ו- `BSD date` , שמציעות תכונות שונות. שיטות אלטרנטיביות יכולות לכלול שימוש בשפות אחרות, כמו Python או JavaScript , או שימוש בספריות חיצוניות.

## ראה גם:
1. Bash-guide https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO-8.html
2. GNU Date manual https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
3. Alternatives in Python https://stackoverflow.com/questions/2490334/simple-way-to-encode-a-string-according-to-a-password