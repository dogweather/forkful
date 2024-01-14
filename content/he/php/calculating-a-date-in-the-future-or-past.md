---
title:                "PHP: חישוב תאריך בעתיד או בעבר"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##למה

כתיבת קוד המתאר תאריך בעתיד או בעבר צריך כיוון נווט, כמו לדוגמא פתרונות עבור בעיות כמו הפסקה לבידוד בזמן. זה גם עשוי להיות מועיל כאשר עובדים עם יישומי משתמש, כמו לדוגמא להציג תאריכים מסוימים בתוך לוח הודעות או לתאריך אירועים פסטיבליים.

##איך לעשות זאת

בשפת PHP ישנם מספר פונקציות המאפשרות לנו לחשב תאריכים בעתיד או בעבר. לדוגמא, נוכל להשתמש בפונקציה `strtotime()` כדי להמיר תאריך מסוים לתאריך עתידי או תאריך עברי:

```PHP
// תאריך עתידי בפורמט תאריך לועזי
$future_date = strtotime('+1 week');
echo date('Y-m-d', $future_date); // 2019-10-25


// תאריך עברי בפורמט תאריך לועזי
$past_date = strtotime('-2 years');
echo date('Y-m-d', $past_date); // 2017-10-18
```

ניתן גם להשתמש בפונקציות נוספות כמו `mktime()` ו- `date_add()` כדי להתאים את התאריך לצרכים שלנו.

##עושים תהליך

לחישוב תאריך בעתיד או בעבר ניתן להשתמש במגוון של פונקציות ולשלבן על מנת לקבל את התוצאה הרצויה. חשוב להתחשב בפורמטי תאריך ובטווח הזמן שמעניין אותנו (ימים, שבועות, חודשים, שנים). ניתן גם להשתמש בתנאים ובלולאות כדי לפענח תאריכים מסורבים כגון ימי שבת או חגים.

##ראו גם

- [למדו עוד על פונקציות תאריך ב-PHP](https://www.php.net/manual/en/ref.datetime.php)
- [תיעוד לפונקציית `strtotime()`](https://www.php.net/manual/en/function.strtotime.php)
- [פתרונות לבעיות נפוצות בתאריכים ב-PHP](https://www.php.net/manual/en/datetime.formats.relative.php