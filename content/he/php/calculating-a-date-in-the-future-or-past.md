---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "PHP: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
חישוב תאריך בעתיד או בעבר הוא פשוט למצוא תאריך שהוא X ימים, שבועות, חודשים או שנים לפני או אחרי תאריך מסוים. מתכנתים מנצלים את זה ליישום משימות כמו חישוב תאריך התפוגה, תזכורות, ועוד.

## איך:
בקוד PHP ניתן לחשב את התאריכים הללו באמצעות פעולת תאריך. הנה דוגמא:

```PHP 
<?php
$date = new DateTime('2021-01-01');
$date->add(new DateInterval('P1M'));
echo $date->format('Y-m-d');
?>
```

הפלט של הדוגמה הזו יהיה:

```PHP 
"2021-02-01"
```

זה מחשב תאריך שהוא חודש לאחר התאריך המסוים, במקרה הזה, 1 בינואר 2021.

## בחפיפה עמוקה יותר:
חישובים אלו פותחו כבר בתיקוני קוד PHP המוקדמים ובהמשך התוספים והמשתפרים.  ישנן שיטות חלופיות לחשב תאריכים בעתיד או בעבר, כמו שימוש בספריות חיצוניות או שימוש בfgetdate.
בנוסף, ניתן להוסיף ולהסיר כמויות זמן שונות כמו שנים, ימים, שעות, דקות ושניות מאובייקט DateTime.

## ראה גם:
MySQL DATE_ADD(): https://www.w3schools.com/sql/func_mysql_date_add.asp
Python datetime timedelta: https://docs.python.org/3/library/datetime.html#timedelta-objects
Java add(): https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html#add-int-int-