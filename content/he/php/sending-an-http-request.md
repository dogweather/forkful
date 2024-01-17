---
title:                "שליחת בקשת http"
html_title:           "PHP: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

### מה ולמה?
שלחת בקשת HTTP פשוט ומביך כשאתה מדבר על זה, אז מה זה בעצם? נכון לעכשיו, האינטרנט הוא רחב וכולם רוצים לקבל את המידע שהם צריכים מהר ובלי אפס טרחה. לשלוח בקשת HTTP היא דרך קלה ובסיסית לפנות לשרתים באינטרנט ולקבל את המידע שאנחנו צריכים.

### איך לעשות זאת?
אם אתם מתכנתים, יש סיכוי טוב שתידעו אפילו לפני שקראתם את המאמר הזה מהן הפקודות הבסיסיות לשליחת בקשת HTTP. כאן נראה כיצד לעשות זאת בעזרת PHP:

```
<?php
$ch = curl_init();
 
// שם כתובת האתר
curl_setopt($ch, CURLOPT_URL, "https://www.example.com/");
 
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
 
// שלח בקשת GET
curl_setopt($ch, CURLOPT_HTTPGET, 1);
 
$data = curl_exec($ch);
 
curl_close($ch);
 
echo $data;
?>
```

בקוד הזה, אנו משתמשים בפונקציות curl_init, curl_setopt, curl_exec ו- curl_close כדי לעשות את הפעולות הבסיסיות לשליחת בקשת HTTP. שימו לב שאנו משתמשים בפונקציות מבני PHP כדי להכמיס את הבקשה לשרת האינטרנט.

### טיול עמוק
השלם היסטורי של שימוש בשרתים באינטרנט מיוחד ומעניין, אך אנחנו לא נדבר על כך כאן. במקום זאת, נסתכל על כמה אלטרנטיבות לשליחת בקשת HTTP דרך PHP.

אחת האלטרנטיבות הנפוצות היא באמצעות פונקציות פנימיות של PHP - file_get_contents ו- fopen. אל תשתמשו בפונקציה הזו עבור בקשות לאתרים גדולים ועמוסים, כי תתקעו את השרת.

כמו כן, כדאי לציין שישנן גם ספריות שלישיות שמיועדות לעזור לפתור בעיות של שליחת בקשת HTTP באמצעות PHP. ספריות כמו cURL ו- Guzzle הן פופולאריות ויכולות לסייע לכם לשלוח בקשות בצורה יעילה ומתקדמת.

### ראו גם
- [פונקציות מבניות של PHP](https://www.php.net/manual/en/function.curl-exec.php)
- [הספריה cURL](https://curl.haxx.se/)
- [הספריה Guzzle](http://docs.guzzlephp.org/en/latest/)