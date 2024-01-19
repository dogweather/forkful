---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
השוואת שני תאריכים היא כלי חשוב בעולם התכנות. תOften, we need to determine the order of events, calculate the time elapsed between dates, or simply check the validity. 

## כיצד לעשות:
בקוד ה-Fish Shell הבא, אנו משווים שני תאריכים כדי לבחון מי מהם מקודם. 

```Fish Shell
function compare_dates
  set date1 (date -d $argv[1] "+%s")
  set date2 (date -d $argv[2] "+%s")

  if test $date1 -eq $date2
    echo "Dates are the same"
  else if test $date1 -gt $date2
    echo "$argv[1] is later than $argv[2]"
  else
    echo "$argv[1] is earlier than $argv[2]"
  end
end
```

כאן מהיציאה של הדוגמה:

```Fish Shell
> compare_dates "2022-01-01" "2022-01-02"
2022-01-01 is earlier than 2022-01-02

> compare_dates "2022-01-02" "2022-01-01"
2022-01-02 is later than 2022-01-01

> compare_dates "2022-01-01" "2022-01-01"
Dates are the same
```

## צלילה עמוקה
השוואת תאריכים היא בעצם בעיה מתמטית. יש לה היסטוריה ארוכה, כולל בבניית הלוח שנה עצמו.
אם אנציקלופדיה, אנו יכולים להשתמש בספריות דרדשת חיצוניות או פונקציות מובנות (כמו `date` שב-Fish Shell) לטפל בפרטים אלה.
בפונקציה שהצגנו, אנו משתמשים במערכת הפקודות של UNIX ‎ובטיפול יעיל של פלט פונקציית `date` ‎.

## ראה גם
למידע נוסף על Fish Shell ואיך להשתמש בו, קראו [המדריך הרשמי של Fish Shell](https://fishshell.com/docs/current/index.html).
לומדים את המונחים בפיתוח קז'ואלי? המשיך לקרוא [באינטרנט](https://www.codecademy.com/learn/learn-the-command-line).
העמיקו עם ה-[UNIX ‎command line documentation‎](http://man7.org/linux/man-pages/man1/date.1.html).