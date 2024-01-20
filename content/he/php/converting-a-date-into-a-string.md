---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
המרת תאריך למחרוזת ב-PHP היא תהליך של יצירת מחרוזת שמייצגת תאריך. מתכנתים עושים את זה כדי להציג, לשמור או לשלוח תאריכים בצורה נוחה ובלתי תלויה בפורמט תאריך מסוים.

## איך:
הנה דוגמאות קוד של איך להמיר תאריך למחרוזת ב-PHP:

```PHP
<?php
$date = new DateTime();
echo $date->format('Y-m-d H:i:s');
?>
```
פלט הדוגמה:

```PHP
"2022-11-12 23:45:10"
```

## צלילה עמוקה: 
המרת תאריך למחרוזת הפכה להיות נפוצה ב-PHP מאז הגרסה 5.2.0, ששוחררה ב-2006, כאשר הוצגה המחלקה DateTime. כמו כן, ישנם שיטות אלטרנטיביות לעשות זאת, אך הן לא תמיד מומלצות מאחר שהן עשויות להיות תלויות-פלטפורמה. במידה ואתה רוצה להשתמש בן format() של DateTime, דע שישנן הרבה אופציות שניתן להשתמש בהן לצורך עיצוב התאריך.

## ראה גם:
בקישור הבא תמצא מידע נוסף על איך להמיר תאריך למחרוזת ב-PHP:
- [PHP Manual: DateTime::format](https://www.php.net/manual/en/datetime.format.php)

אם אתה מעוניין לגלות עוד על המחלקה DateTime, פה גם יש מידע מעניין:
- [PHP Manual: DateTime](https://www.php.net/manual/en/class.datetime.php)