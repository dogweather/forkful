---
title:                "Fish Shell: השוואת שתי תאריכים"
simple_title:         "השוואת שתי תאריכים"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה

השוואת תאריכים היא פעולה חשובה בכתיבת קוד ומאפשרת לנו להתמודד עם תאריכים בצורה יעילה יותר. כתבה זו תסביר לכם איך להשוות שני תאריכים בעזרת שפת התכנות Shell Fish.

## איך לעשות זאת 

כדי להשוות שני תאריכים בשפת Shell Fish, ניתן להשתמש בפקודה "date -d" עם הארגומנט "days". ניתן לראות מגוון רחב של דוגמאות לתאריכים כאן:

```fish
date -d "2021-01-01" "+%F"
```

התוצאה שתחזור היא תאריך בפורמט המבוקש, לדוגמה - "2021-01-01". פקודה זו עוזרת לנו להתמודד עם תאריכים בצורה יעילה וברורה.

## כיוון עמוק

לחלקים מיוחדים ביחס להשוואת תאריכים בשפת Shell Fish ניתן להשתמש בפקודה "date -s" כדי להתאים את התאריכים בצורה ברורה יותר. ניתן לראות דוגמאות נוספות לכאן:

```fish
date -s "2021-01-01 12:00:00"
```

ניתן לשלב גם פקודה זו עם פקודה המשווה בין שני תאריכים כגון "date -d" כדי לקבל תוצאה עדכנית ומדויקת.

## ראה גם

- [תיעוד נוסף על השמת תאריכים כאן](https://fishshell.com/docs/current/cmds/date.html)
- [מדריך לשימוש בשפת Shell Fish](https://fishshell.com/docs/current/tutorial.html)
- [דוגמאות נוספות על השוואת תאריכים כאן](https://linuxhint.com/how-to-compare-dates-in-bash/#t2)