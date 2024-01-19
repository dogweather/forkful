---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
פָּרסִינְג של תאריך ממחרוזת היא לתמרור נתונים ממחרוזת כדי להשיג תאריך שהמחשב יכול להבין. מתכנתים מבצעים את זה כדי לנצל את התאריך באופן שאינו אפשרי כאשר התאריך מגיע כמחרוזת.

## איך ל:
הנה דוגמא לקוד שיבצע זאת ב-Fish shell:

```Fish Shell

set date_string "2021-07-14"
set formatted_date (date -ud $date_string +"%d/%m/%Y")

echo $formatted_date
```

תוצאת הדוגמה היא:
```
14/07/2021
```

## הטבלה 
1. המסגרת ההיסטורית: Fish Shell מתמקד בעיקר בפונקציונליות פשוטה וגבישה. המטרה העיקרית של שפת התכנות הזו היא להקל על שימוש המשתמש.
2. אלטרנטיבות: באלתרנטיבות אחרות כמו bash או zsh, אתה יכול למצוא את אותה התנהגות עם שינויים קלים בדקדוק. זה יכול להיות מתסכל עבור מתכנתים שלא מבינים את השפה המקורית.
3. האם ידעת? בהינתן שFish Shell הוא shell אינטראקטיבי, הוא משתמש ב "innerHTML" במקום "innerHTML" אחר בדיוק כמו שJavaScript עושה.

## ראה גם:
1. [דוקומנטציה של Fish Shell](https://fishshell.com/docs/3.0/index.html) 
2. [טיפים לשפת תכנות Fish Shell](https://github.com/jorgebucaran/awesome-fish) 
3. [ספר יבש Fish Shell](https://fishshell.com/docs/3.1/tutorial.html) 

שימו לב, Fish Shell היא שפת תכנות פשוטה ועוצמתית שכדאי להכיר!