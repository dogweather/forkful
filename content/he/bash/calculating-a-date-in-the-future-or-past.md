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
חישוב תאריך מתוכנן בעתיד או בעבר הוא פעולה שמאפשרת לנו לחשב תאריכים במהירות ובדיוק. תכניותיסטים מבצעים פעולות כאלו כדי לתכנן תאריכים של אירועים מתוכננים או להשתמש בתאריכים כקלט לתוכנה כדי לבצע חישובים נוספים.

## איך לעשות:
לדוגמה, כדי לחשב תאריך שיהיה 7 ימים מהיום, אנחנו יכולים להשתמש בקוד הבא:

```Bash
date -d "+7 days"
```

פלט הפקודה יהיה התאריך שיהיה 7 ימים מהיום הנוכחי.

## לחקור יותר:
מקור החישובים של תאריכים הוא ישן מאוד ומורכב ונתמך בזמן רב. אחת התכונות המובילות של תאריך הוא הכלל של דודסון, אשר הוגדר על ידי בן שלמה דודסוןים בשנת 1582. כיום ישנן פתרונות רבים עבור חישובי תאריך, כולל מספר פקודות לבניית תאריכים לפי תבניות מועדפות.

## לראות גם:
למידע נוסף על חישובי תאריך ב-Bash, ניתן להציץ במקורות האלה:
- [פקודת date בביקורת בלוגית](https://www.,בלוגיםינלי/blog/2020/01/introduction-to-the-date-command-in-bash/)
- [חישוב תאריך עם פקודת date](https://www.linuxnix.com/calculate-date-with-date-command-in-linux/)
- [מדריך לתאריך ושעה ב-Bash](https://www.shellhacks.com/date-command-format-options-examples/)