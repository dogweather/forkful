---
title:    "Fish Shell: השוואת שתי תאריכים"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה

אנשים יכולים להשתמש בשפת תכנות Fish Shell כדי להשוות שתי תאריכים כדי לממש פקודות תנאי המבוססות על קריטריוני תאריך. החשיבה הקטנה שלה בקונטקסט של השפת תכנות כולה הופכת את זה לתהליך יישומי חשוב לתנאים פעילים.

## איך לעשות זאת

```Fish Shell
# ייבוא שתי תאריכים למשתנים
set date1 2020-01-01
set date2 2020-02-01

# השוואת התאריכים באמצעות פקודת test
if test $date1 = $date2
    # התאריכים בולטים מחדירה זוויות
    echo "תאריכים זהים"
else if test $date1 != $date2
    # התאריכים אינם זהים
    echo "תאריכים שונים"
end
```

הפלט של הקוד הנ"ל יהיה:

```Fish Shell
$ תאריכים שונים
```

## כיוון עמוק

כשאנו משווים שני תאריכים בשפת Fish Shell, הם מושווים כמחרוזות. זה יעיל לתכנות פקודות תנאי כמו "אם התאריך הוא לפני תאריך מסוים". עצם לבדו, שפת התכנות שלנו לא תאפשר לנו להשוות תאריכים מדוייקים. ישנם פתרונות נוספים לביצוע השוואה מדוייקת של תאריכים, כגון שימוש בספרייה שלנו או בשימוש בכלי תאריך נפרד.

## ראה גם

- [תיעוד של Fish Shell על פקודת test](https://fishshell.com/docs/current/cmds/test.html)
- [סרטון הדרכה על פקודת test ב Fish Shell](https://www.youtube.com/watch?v=I3hrlV0cS1s)
- [מדריך לשפת תכנות Fish Shell](https://fishshell.com/docs/current/tutorial.html)