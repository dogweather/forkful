---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?

ישיבת התאריך הנוכחי היא שיטה להשיג את התאריך וזמן הרגע תוך שמירה עליו במשתנה. מתכנתים עושים את זה כדי לעקוב אחרי התאריכים של אירועים בתוכנית כמו יצירת משתמש או עדכון רשומה.

## איך לעשות:

PHP מאפשר השגת התאריך הנוכחי באמצעות הפונקציה date().

```PHP
<?php
  echo date("Y-m-d H:i:s");
?>
```

פלט:

```
2022-03-29 14:31:57
```

## צלילה עמוקה:

צלילה לעומק הפונקציה `date()`: במהותה, היא מציגה תבנית מחרוזת מדויקת להצגת התאריך הנוכחי, תלוי בהתאמה לאזור הזמן.

אלטרנטיבות: PHP מציעה פונקציות חלופיות כמו `getdate()` אשר מחזירה מידע מפורט יותר על התאריך הנוכחי, ו-`time()` שמחזירה את הזמן הנוכחי בשניות מאז 1970.

פרטים נוספים: בדרך כלל, מומלץ להשתמש בהגדרות אזור הזמן המקומי לקבלת התאריך והשעה הנוכחיים. אם האזור הזמני לא מוגדר, PHP ישתמש באזור הזמן שהוגדר בהגדרות השרת.

##  ראה גם:

שינוי אזור הזמן PHP: [המדריך הרשמי](https://www.php.net/manual/en/function.date-default-timezone-set.php)

הפונקציה `getdate()`: [המדריך הרשמי](https://www.php.net/manual/en/function.getdate.php)

הפונקציה `time()`: [המדריך הרשמי](https://www.php.net/manual/en/function.time.php)