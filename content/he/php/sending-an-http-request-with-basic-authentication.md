---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "PHP: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי היא פעולה נפוצה בתכנות המאפשרת לצד מתכנת להתחבר לשרת תוך שימוש בשם משתמש וסיסמה. הסיבה לכך היא כי אימות בסיסי מספק שכבת אבטחה נוספת כדי להגן על נתוני המשתמש ולמנוע גישה ללא מורשה לשרת.

## איך לעשות:
דוגמאות קוד ופלט מוצגים בכבודת `PHP…` 

```php
// הגדרת משתנים לשילוח הבקשה
$url = 'http://www.example.com/api/users';
$username = 'myusername';
$password = 'mypassword';

// ייצור בקשת HTTP עם אימות בסיסי
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, $url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, $username . ':' . $password);

// שליחת הבקשה והצגת התוצאה
$response = curl_exec($ch);
curl_close($ch);
echo $response;
```

פלט:
```
[
  {
    "id": 1, 
    "name": "John Smith", 
    "email": "johnsmith@gmail.com"
  }, 
  {
    "id": 2, 
    "name": "Jane Doe", 
    "email": "janedoe@gmail.com"
  }
]
```

## כיווץ עמוק:
- היסטוריה: אימות בסיסי נוצר עבור פרוטוקול האינטרנט Telnet בשנות ה-70 כדי לאפשר למשתמשים להתחבר לסיסטם אווירוע עם שם משתמש וסיסמה.
- אלטרנטיבות: אימות בסיסי הוא רק אחת ממגוון האפשרויות לאימות שרתים באינטרנט, כולל OAuth, JWT ו- HTTPS. כל אחת מהאלטרנטיבות מספקת רמות שונות של אבטחה ויעילות.
- פירוט היישום: שליחת בקשת HTTP עם אימות בסיסי ב-PHP יכולה להתבצע באמצעות שימוש בפונקציה `curl_setopt()`, תוכניות צד שלישי כמו Guzzle והשתמשויות כמו Basic Auth for WordPress.

## ראו גם:
- [שליחת בקשת HTTP ב-PHP עם CURL](https://www.php.net/manual/en/curl.examples-basic.php)
- [מדריך לאימות בסיסי באמצעות CURL עבור מתכנתים](https://developer.okta.com/blog/2019/05/31/basic-http-authentication-with-curl)