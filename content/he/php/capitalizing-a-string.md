---
title:                "הפיכת מחרוזת לאותיות רישיות"
date:                  2024-01-19
html_title:           "Bash: הפיכת מחרוזת לאותיות רישיות"
simple_title:         "הפיכת מחרוזת לאותיות רישיות"

category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

בעברית: עיצוב מחרוזת לאותיות גדולות הוא שינוי של טקסט כך שכל האותיות בו יהיו גדולות. תוכניתנים עושים זאת לצורך אחידות, קריאות או עמידה בדרישות פורמט.

## איך לעשות:

כדי להפוך מחרוזת לאותיות רישיות בקלות, השתמש בפונקציה `strtoupper`. פה יש דוגמה:

```php
<?php
$text = "שלום עולם";
$capitalizedText = strtoupper($text);

echo $capitalizedText; // תוצאה: שלום עולם באותיות רישיות
?>
```

שים לב: בעברית אין הבדל בין אותיות קטנות לגדולות, אבל זה יעבוד על אנגלית ושפות אחרות.

## עיון מעמיק:

השימוש באותיות גדולות במחשבים מתחיל בשנות ה-60 וה-70 עם תקני ASCII ו-EBCDIC שהיו מבוססים רק על אותיות רישיות. כיום, הצורך באותיות גדולות משמש גם לעיצוב ויזואלי וגם לתכנות קייס-סנסיטיב. ב-PHP, למשל, יש פונקציות נוספות כגון `strtolower` להפוך לאותיות קטנות, ו`ucfirst` לראשית גדולה. כל הפונקציות האלו משחקות עם היצוג הבינארי של האותיות במחשב.

בפיתוח ווב ואפליקציות, חשוב לזכור שמעבר ל-PHP, פעמים רבות מבוצעת עיבוד ועיצוב של מחרוזות בצד הלקוח (עם JavaScript, למשל), או בשכבת הנתונים (עם SQL או נוסחאות של מסד נתונים).

## ראה גם:

- [PHP strtolower](https://www.php.net/manual/en/function.strtolower.php)
- [PHP ucfirst](https://www.php.net/manual/en/function.ucfirst.php)
- [PHP ucwords](https://www.php.net/manual/en/function.ucwords.php)
- [ASCII Table and Description](https://www.asciitable.com/)
