---
title:                "המרת תאריך למחרוזת"
html_title:           "Fish Shell: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה

כי מצביעים כונן Linux או Unix את יום גישה קבועים למחשב ומשתמשים בפקודות כדי להציג את התאריך הנוכחי. עשה את זה בצורה ברורה, קלה ומהירה עם פקודת Fish Shell המתקדמת.

## איך

```Fish Shell
set mydate (date +"%Y%m%d")
echo $mydate
```

מסתבר שהפקודה שנקראת בשורה הראשונה מציגה את התאריך הנוכחי בפורמט הנכון לך מה ערכי התאריך. תוכל לשנות את הפורמט כדי שיתאים לדרישות שלך. בצורה דומה, אתה יכול להשתמש בפקודה `date -d` כדי להציג תאריכים שונים מאלו שמקבלים בטבלה או בספריה משתנים.

## עומק ים
מפרטים אתכם על כיצד להפוך את אופני הקורסורים של תאריך אנודינמי למחרוזת בפורמט מותאם אישית. יחד עם פלט לא ברור בהתאם לימים שנכנסו, אתה יכול להשתמש בפקודה `date` כדי להציג תאריך חלק מתאריך.

תרצה לקרוא את זה בקוד יותר ממפרט? אז ננדה שמן בכתובת https://fishshell.com/docs/current/index.html אנשי קורס וידוא חזק מודעת הקשורים לערכת לשירת Fish Shell.

## ראה גם

- https://linuxize.com/post/bash-date-command/
- https://linux.die.net/man/1/date
- https://www.geeksforgeeks.org/eval-command-in-linux-with-examples/