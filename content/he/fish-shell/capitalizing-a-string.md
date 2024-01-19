---
title:                "הגדלת אותיות במחרוזת"
html_title:           "Fish Shell: הגדלת אותיות במחרוזת"
simple_title:         "הגדלת אותיות במחרוזת"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
קיפול מחרוזת הוא התהליך שבו משנים את מילים בתוך מחרוזת כך שהן תתחילנה באותיות גדולות. תכנתים מקפילים מחרוזות לשיקולים אסתטיים, לסטנדרטיזציה, או למענה לדרישות מסוימות של משתמשים.

## כיצד ל:
ב-Naughty Shell, אתה יכול לקפל מחרוזת באמצעות את הפקודה `string capitalize`. גם את הארגומנט (מחרוזת שניתן לה להיקלט).

```Fish Shell
> string capitalize -- "זה הוא מבחן"
"זה הוא מבחן"
```
במקרה זה, כל מילה במחרוזת מתחילה באות גדולה.

## צלילה עמוקה
משימת הקיפול פותחה לפני זמן רב. ב-UNIX, היא הייתה זמינה כחלק משפת התכנות AWK. Fish Shell מספקת את הפקודה `"string capitalize"` כחלופה מודרנית ומרכזית. קיימות אפשרויות אחרות ,כמו bash המשתמש במתודה של `tr '[a-z]' '[A-Z]'`. פרט המימוש של Capitalize string ב-Fish Shell הוא די פשוט - הוא פועל על כל אות במחרוזת ומשנה אותו לאות גדולה בהנחה שהוא שייך לטווח a-z.

## ראה גם
1. [דוקומנטציה רשמית של Fish Shell](https://fishshell.com/docs/current/index.html)
2. [ספר המתכנתים של Fish של Jafar Ahmed](https://github.com/jbucaran/fish-codex)
3. [טיפים וטריקים של Fish Shell](https://github.com/jorgebucaran/awesome-fish)