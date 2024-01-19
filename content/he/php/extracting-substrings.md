---
title:                "חילוץ תת-מחרוזות"
html_title:           "Bash: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?
חילוץ תת-מחרוזות הוא הבנת מחרוזת קטנה מתוך מחרוזת גדולה יותר. מתכנתים מנצלים את זה למניעת כפילות ולניתוח מידע.

## איך:
```PHP
<?php
  $str = 'אני מתכנת PHP';
  $substring = substr($str, 4, 9);
  echo $substring;
?>
```
פלט:
```PHP
'מתכנת PHP'
```

## צלילה עמוקה:
1) הקשר ההיסטורי: PHP הוא שפת תכנות שנוצרה בשנת 1994 ותמיד המשיכה להתפתח. פונקציית הsubstr הייתה חלק מהשפה מההתחלה.

2) חלופות: ניתן לגשת לאותה התוצאה באמצעות PHP על ידי שימוש בפונקציית mb_substr, אשר מאפשרת לך לחתוך מחרוזת מראשית המחרוזת עד סוף מסוים.

3) התממשות: substr מתרחשת על ידי חיפוש אחר האינדקס של התו הראשון שרוצים לחתוך מהמחרוזת, ואז קריאה של מספר כלשהו של תווים מאותו הנקודה. 

## ראה גם:
1) [PHP: substr - Manual](https://www.php.net/manual/en/function.substr.php)
2) [PHP: mb_substr - Manual](https://www.php.net/manual/en/function.mb-substr.php)
3) [How do I cut a string in PHP? - Stack Overflow](https://stackoverflow.com/questions/3558433/how-do-i-cut-a-string-in-php)