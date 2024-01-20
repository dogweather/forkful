---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה & למה?
שליחת בקשת HTTP עם אימות בסיסי הוא תהליך שבו אנו שולחים בקשת HTTP ומתוך הבקשה מסמנים את הדרישה לאימות, בדעיך של שם משתמש וסיסמה. מתכנתים בוחרים לבצע זאת כדי להאבטח גישה למשאבים או גישה למסלולים מסוימים באפליקציה שלהם.

## איך לעשות:
נוכל לבצע את זה ב-PHP באמצעות ספריית cURL. להלן דוגמה על כיצד לשלוח בקשת HTTP עם אימות בסיסי.

```PHP
$ch = curl_init();

curl_setopt($ch, CURLOPT_URL, 'http://example.com');
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
curl_setopt($ch, CURLOPT_USERPWD, 'username:password');

$result = curl_exec($ch);

if(curl_errno($ch)){
    echo 'Error:' . curl_error($ch);
}
curl_close($ch);
```
בקוד זה, אנחנו משתמשים ב-`curl_setopt` כדי להגדיר את שם המשתמש והסיסמה לבקשת ה-HTTP. התשובה מהשרת מאוחסנת ב-`$result`.

## דיפ דייב:
שימוש באימות בסיסי הוא נפוץ במהלך ההיסטוריה של הפרודקול HTTP. זוהי טכניקה בסיסית אך יעילה לאבטחת גישה, אך לא תמיד היא הבחירה הטובה ביותר. ראוי לשקול שימוש בשיטות אחרות, המחייבות יישומים כמו OAuth או מחזיקי מפתח API. שימו לב שבשיטה זו, שם המשתמש והסיסמא משוגרים בצורה בלתי מוצפנת, שמזעזעת במקרים שבהם המידע אינו מוגן בצורה מוצפנת.

## ראה גם:
1. [HTTP Authentication](https://tools.ietf.org/html/rfc7235)
2. [PHP: Hypertext Preprocessor - Official Documentation](https://www.php.net/manual/en/book.curl.php)
3. [Understanding Basic HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication).