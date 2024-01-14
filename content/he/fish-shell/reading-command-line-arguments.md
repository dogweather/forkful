---
title:    "Fish Shell: קריאת ארגומנטים מפקודת הפקודה"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# למה

קריאת פרמטרי שורת הפקודה מאפשרת לנו לתכנת סקריפטים ותוכניות בצורה יעילה יותר. היא מאפשרת לנו לשלוט על הפעולות של התוכניות שלנו ולהעביר להן מידע יעיל נוסף.

# איך לעשות זאת

הקובץ `fish` מאפשר לנו לקבל את פרמטרי השורת הפקודה בצורה יעילה ונוחה. אפשר להשתמש בפקודה `argv` כדי לקרוא את הפרמטרים מתוך הקובץ בפקודת shell. לדוגמה:

```
Fish Shell
salmon.fish
```

כאן, קובץ השלומית `salmon.fish` מקבל את הפרמטר `Fish Shell` כפרמטר נכנס.

# צלילה עמוקה

כאשר מתקדמים בתכנות פשוט, קריאת פרמטרי שורת הפקודה היא כלי חיוני להבנה טובה יותר של פקודות shell ולבנות סקריפטים יעילים יותר. כאשר משתמשים בפקודת `argv`, ניתן להשתמש בתנאים ולבצע פעולות מתקדמות יותר בהתאם לפרמטרים שהתקבלו. ישנם גם פקודות מתקדמות כמו `getopts` המאפשרות לנו לטפל בפרמטרים בצורה יעילה יותר.

# ראה גם

* דרך נוחה לקרוא פרמטרי שורת הפקודה בשפת Fish Shell - <https://fishshell.com/docs/current/tutorial.html#reading-command-line-arguments>
* פקודת `getopts` בדיוק - <https://fishshell.com/docs/current/cmds/getopts.html>
* תמיכה נוספת בפרמטרים בשפת Fish Shell - <https://fishshell.com/docs/current/commands.html#argv>