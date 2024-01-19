---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

##מה זה & למה?
שוואה בין שני תאריכים היא השוואה פשוטה וישירה בין תאריכים על פי המשתנים: יום, חודש, שנה, שעה, דקה, שניה. תכנתים משתמשים בפעולה זו לביצוע חישובים תאריך משולבים, ניהיל מידע מוזמנים, ועוד.

## כיצד ל...
```PHP
<?php
$date1 = date_create('2020-04-21');
$date2 = date_create('2021-06-14');

$difference = date_diff($date1, $date2);

echo $difference->format('%R%a ימים');
?>
```
פלט לדוגמה הוא: "+420 ימים", מציין ההפרש ביניהם.

##צלילה עמוקה 
הפונקציה date_diff התווספה ל-PHP בגרסה 5.3.0 ומאז היא הבחירה המועדפת להשוואה בין תאריכים. ישנן אפשרויות אחרות, כמו להמיר את התאריכים לזמן Unix ולהשוואתם, אך אין זה כל כך זמין כמו date_diff. בפנים המנוע, date_diff משווה יחידת תאריך מסוימת משני תאריכים, ולא מחשב את ההפרש בשניות וממירו בחזרה לתאריך.

##ראה גם
[PHP Date/Time Manual](https://www.php.net/manual/en/book.datetime.php)

[PHP Date/Time Functions](https://www.php.net/manual/en/ref.datetime.php)

האישורים של PHP על שימוש בתאריכים ושעות רבים ומגוונים, ומהווה מדריך קודד למתכנתים בכל הרמות. הם נותנים דוגמאות מרגעים מרובים ומראים את הדרך להתמודד עם עיוותי זמן ופונקציונאליות משולבת.