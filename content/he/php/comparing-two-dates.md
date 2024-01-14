---
title:                "PHP: השוואת שתי תאריכים"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

# למה

בעולם התכנות, ייתכן ותתקלו במקרים בהם יהיה עליכם להשוות שתי תאריכים. השוואת תאריכים היא כלי חשוב בתכנות שיש להשתמש בו בכדי להתאים מתאם מהיר וקל להבנה שלבין שני תאריכים שונים.

# כיצד לבצע

בכדי להשוות שתי תאריכים ב- PHP, ניתן להשתמש בפונקציה המובנית strtotime כדי להמיר את התאריך לפורמט ידוע יותר כמו תאריך Unix timestamp. לאחר מכן, ניתן להשתמש בפונקציה המובנית date כדי להציג את התאריך בפורמט המבוקש. נהלך קדימה לדוגמאות ולפלט המתאים בתוך פסקת הקוד שלנו.

```PHP
<?php
$date1 = strtotime("2020/10/01");
$date2 = strtotime("2020/12/25");
echo date("d/m/Y", $date1); // Output: 01/10/2020
echo date("d/m/Y", $date2); // Output: 25/12/2020
?>
```

# פינות עמוקות

כאשר משווים שני תאריכים, חשוב להתחשב במקרים מיוחדים כמו שנת ליקוי, שנת פברואר ועוד. כדי להתמודד עם מקרים אלו, ניתן להשתמש בפונקציות נוספות כמו checkdate ו cal_days_in_month. חשוב גם לבדוק כי הערכים שהוכנסו מייצגים באמת תאריכים וניתן להשתמש בדוגמאות שנמצאות מתחת כדי לוודא כי הפסקלאות שלכם עובדות כצפוי.

# ראו גם

- https://www.php.net/manual/en/function.strtotime.php
- https://www.php.net/manual/en/function.date.php
- https://www.php.net/manual/en/function.checkdate.php
- https://www.php.net/manual/en/function.cal-days-in-month.php