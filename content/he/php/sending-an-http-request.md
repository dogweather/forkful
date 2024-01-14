---
title:                "PHP: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה

מדוע לפתח מבחנת התקשרות HTTP יכולה להיות חשובה למתכנתי PHP? כמו HTML, CSS ו-JavaScript, HTTP גם היא חלק מהחבילה הפתוחה של הפרוטוקולים הנסמכים עליה אינטרנט, וכמו כן יכולה לספק מידע ותוכן לאתרים ויישומי אינטרנט.

## איך לעשות זאת 

ניתן לשלוח בקשת HTTP על ידי כתיבת קוד PHP פשוט בתוך *הדפדפן שלכם*. נדון בכמה מקרים נפוצים:

```PHP
<?php 

// כתיבת בקשת GET פשוטה

$response = file_get_contents('http://example.com'); 
echo $response; // מדפס את תוכן האתר המבוקש

// כתיבת בקשת POST לשרתים באינטרנט (לדוגמה, מאגרי נתונים)

$database = ['name' => 'דויד', 'age' => 33]; // יצירת משתנה שמכיל מידע עבור הבקשה
$options = ['http' => ['method' => 'POST', 'content' => http_build_query($database)]]; // יצירת מעטפת הקבצים של הבקשה
$context = stream_context_create($options); // יצירת קשר עם הבקשה
$result = file_get_contents('http://example.com', false, $context); // שליחת בקשה וקבלת תשובה
echo $result; // מדפיס את התשובה מהשרת
```

## טיול עמוק

גילוי הכוחות של HTTP לא נגמר רק בשליחת וקבלת בקשות פשוטות. ניתן להתכתב עם שרתים מרובים ושפת לסיסמאות על ידי שימוש בפונקציות נוספות כמו `fsockopen()` ו-`stream_socket_client()`. ניתן גם להוסיף כותרות נוספות לבקשת HTTP, כגון `User-Agent` ו-`Accept-Encoding`, על מנת ליצור בקשות מתוחכמות יותר.

## ראו גם

למידע נוסף על התקשורת בין PHP לשרתי האינטרנט, ניתן לקרוא את תיעוד ה- PHP על הפונקציות המובנות `file_get_contents()` ו-`http_build_query()`: 

- [PHP: file\_get\_contents - Manual](https://www.php.net