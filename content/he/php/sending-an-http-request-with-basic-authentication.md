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

## למה 

# אותנו אנשים שולחים בקשות HTTP עם אימות בסיסי 

שליחת HTTP בקשה עם אימות בסיסי היא פעולה נפוצה ביישומי התוכנה המבוססים על רשת. זהו תהליך שמאפשר למשתמשים להתחבר ולגשת למרבי התוכן הזמינים במקומות שונים של האינטרנט. האימות הבסיסי מספק שכבת אבטחה פשוטה כדי להבטיח כי רק משתמשים המורשים יכולים לגשת למידע.

## איך לעשות 

```
<?php 
// ייבוא הספריה עבור הבקשות ה-HTTP
require 'vendor/autoload.php'; 

// יצירת אובייקט עבור הפנייה 
$request = new \GuzzleHttp\Psr7\Request('GET', 'https://www.example.com/api/users', [
    'Auth' => ['username', 'password'] // שם משתמש וסיסמה לאימות בסיסי 
]);

// שליחת הבקשה והקבלת התגובה
$response = \GuzzleHttp\Client->send($request);

// הדפסת תוכן התגובה 
echo \GuzzleHttp\Stream\BufferStream(@$response->getBody()); 
```

### תגובה:

```
{
    "status": "success", 
    "data": {
        "id": 123, 
        "name": "John Doe", 
        "email": "john@example.com"
    }
}
```

## מעמיקים 

שליחת בקשת HTTP עם אימות בסיסי מתבצעת על ידי הוספת כותרת ה- "Authenticate" בכותרת הבקשה. כותרת זו מכילה שתי כרטיסיות, אחת עבור השם המשתמש ואחת עבור הסיסמה של המשתמש. הערך של הכרטיסיות מודרת עם נקודת רישון (:), כמו "username:password". מומלץ להשתמש בכתבים 64-ביט ליצירת ערך אימות מוצפן בסיסי64.

## ראה גם 

[חבילת Guzzle HTTP](https://packagist.org/packages/guzzlehttp/guzzle) - ייבוא ספריית Guzzle כדי לאפשר שליחת בקשות HTTP בקלות ובאמינות.

[מדריך Laravel Passport על אימות בסיסי](https://laravel.com/docs/6.x/passport#introduction) - מדריך של לעשות אימות בסיסי באמצעות חבילת Passport בלר