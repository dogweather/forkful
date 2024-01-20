---
title:                "שימוש בביטויים רגולריים"
html_title:           "Bash: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?
תכנות עם ביטויים רגולריים (Regular Expressions) זה לחפש, להחליף, ולנתח טקסט. מתכנתים משתמשים בזה כדי לעבוד יעיל עם מחרוזות ולבצע משימות מורכבות בקלות.

## איך לעשות:
כדי לחפש מחרוזת שמתאימה לביטוי רגולרי נשתמש בפונקציה `preg_match`:
```PHP
<?php
$pattern = "/ברוך/i";
$text = "ברוך הבא למדריך PHP!";
if (preg_match($pattern, $text)) {
    echo "מצאנו תאימות!";
} else {
    echo "אין תאימות.";
}
// פלט: מצאנו תאימות!
?>
```
להחלפת טקסט נשתמש ב-`preg_replace`:
```PHP
<?php
$pattern = "/שלום/";
$replacement = "שלום עולם";
$text = "שלום!";
echo preg_replace($pattern, $replacement, $text);
// פלט: שלום עולם!
?>
```

## טבילה עמוקה
ביטויים רגולריים הם פיצ'ר עתיק שהתפתח משנות ה-50. אלטרנטיבות כוללות פונקציות כגון `strpos()` ו-`str_replace()`, אבל הם פחות גמישות. ב-PHP, פונקציות שמתחילות ב-`preg_` מבוססות על הספריית PCRE (Perl Compatible Regular Expressions), שמאפשרת עבודה עם ביטויים רגולריים תואמי Perl.

## ראו גם:
- [מדריך ביטויים רגולריים ל-PHP באתר php.net](https://www.php.net/manual/en/book.pcre.php)
- [טסטר לביטויים רגולריים](https://regex101.com/)
- [איך לכתוב ביטויים רגולריים יעילים](https://www.rexegg.com/)

זהו, שימוש בביטויים רגולריים ב-PHP בקצרה. רוצה לדעת יותר? היכנסו לקישורים שלעיל.