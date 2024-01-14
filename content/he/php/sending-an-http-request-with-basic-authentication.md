---
title:                "PHP: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

חשיבות הפניות HTTP עם אימות בסיסי

כתבות הפניות HTTP עם אימות בסיסי הן חלק חשוב מתכנות ווב. כאשר אתם עובדים עם אתרים או אפליקציות, ייתכן ותצטרכו לשלוח בקשת HTTP עם אימות בסיסי כדי לקבל גישה למידע מוגן. זהו אופציה נפוצה גם כאשר אתם משתמשים ב-API או כאשר אתם מנסים לגשת לאתרים באמצעות תוכנות פתוחות. להלן נסביר את חשיבות השימוש באימות בסיסי בפניות HTTP ואיך לעשות זאת בפעולה.

כיצד לשלוח פנייה עם אימות בסיסי ב-PHP

כדי לשלוח בקשת HTTP עם אימות בסיסי ב-PHP, תצטרכו לעבוד עם הפונקציה curl. הנה דוגמא של שליחת בקשה GET לאתר מוגן באמצעות אימות בסיסי:

```PHP
<?php 
// URL ופרטי התחברות
$url = 'https://example.com/api/data';
$username = 'my_username';
$password = 'my_password';

// הכנת המופע של curl והגדרת האופציות
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, $url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, "$username:$password");

// שליחת הבקשה
$response = curl_exec($ch);
curl_close($ch);

// הדפסת תוצאת הבקשה
echo $response;
?>
```

כרגע אנו משתמשים בפונקציות curl כדי לבצע את הפנייה עם אימות בסיסי. לפני שנשלח את הבקשה, אנו מגדירים את הפרמטרים הנחוצים לפונקציה, כולל ה-URL של האתר, שם המשתמש והסיסמה. על מנת לבצע אימות בסיסי, נקבע את אפשרות CURLAUTH_BASIC כפרמטר מתאים לפקודת ה-curl_setopt. בכתבת זו אנו מנתחים את המשתנים של התחברות, אך ייתכן שבבקשתכם למקבל תוצאות כלשהן אשר