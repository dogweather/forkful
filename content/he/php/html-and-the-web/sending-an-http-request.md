---
date: 2024-01-20 18:00:50.222384-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05DE\u05D0\
  \u05E4\u05E9\u05E8\u05EA \u05DC\u05EA\u05DB\u05E0\u05D9\u05EA PHP \u05E9\u05DC\u05DA\
  \ \u05DC\u05D3\u05D1\u05E8 \u05E2\u05DD \u05E9\u05E8\u05EA\u05D9\u05DD \u05D0\u05D7\
  \u05E8\u05D9\u05DD - \u05D6\u05D4 \u05DB\u05DE\u05D5 \u05DC\u05E9\u05DC\u05D5\u05D7\
  \ \u05DE\u05E1\u05E8 \u05D1\u05D1\u05E7\u05D1\u05D5\u05E7 \u05D1\u05D9\u05DD \u05D4\
  \u05D3\u05D9\u05D2\u05D9\u05D8\u05DC\u05D9. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\
  \u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\
  \u05D3\u05D9 \u05DC\u05D4\u05E9\u05D9\u05D2 \u05DE\u05D9\u05D3\u05E2, \u05DC\u05D0\
  \u05DE\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD,\u2026"
lastmod: '2024-03-13T22:44:39.475609-06:00'
model: gpt-4-1106-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05DE\u05D0\
  \u05E4\u05E9\u05E8\u05EA \u05DC\u05EA\u05DB\u05E0\u05D9\u05EA PHP \u05E9\u05DC\u05DA\
  \ \u05DC\u05D3\u05D1\u05E8 \u05E2\u05DD \u05E9\u05E8\u05EA\u05D9\u05DD \u05D0\u05D7\
  \u05E8\u05D9\u05DD - \u05D6\u05D4 \u05DB\u05DE\u05D5 \u05DC\u05E9\u05DC\u05D5\u05D7\
  \ \u05DE\u05E1\u05E8 \u05D1\u05D1\u05E7\u05D1\u05D5\u05E7 \u05D1\u05D9\u05DD \u05D4\
  \u05D3\u05D9\u05D2\u05D9\u05D8\u05DC\u05D9."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP"
weight: 44
---

## מה ולמה?
שליחת בקשת HTTP מאפשרת לתכנית PHP שלך לדבר עם שרתים אחרים - זה כמו לשלוח מסר בבקבוק בים הדיגיטלי. תוכניתנים עושים את זה כדי להשיג מידע, לאמת נתונים, להתחבר ל-APIs או לשלוח נתונים למקומות אחרים.

## איך לעשות:

בואו נשתמש ב-cURL שהוא אחד הכלים הפופולריים ב-PHP לשליחת בקשות HTTP.

```PHP
<?php
// הגדרת URL
$url = 'https://api.example.com/data';

// אתחול cURL
$ch = curl_init($url);

// הגדרת אפשרויות לבקשה
curl_setopt($ch, CURLOPT_HTTPGET, true);  // בקשת GET
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);  // אחזור תגובה כמחרוזת

// שליחת הבקשה ואחזור התגובה
$response = curl_exec($ch);

// בדיקה אם הייתה שגיאה
if(curl_errno($ch)) {
    echo 'cURL error: ' . curl_error($ch);
}

// סגירת המשאב cURL
curl_close($ch);

// הדפסת התשובה מהשרת
echo $response;
?>

// דוגמא לפלט שמחזיר הקוד:
// {"success":true,"data": {...}}
```

## תוכן עמוק יותר:
- **הקונטקסט ההיסטורי:** שליחת בקשות HTTP התפתחה מלהיות פעולה מסובכת לפעולה פשוטה ונפוצה עם התפשטות האינטרנט.
- **אלטרנטיבות:** ניתן לשלוח בקשות HTTP גם בשיטות אחרות כמו file_get_contents של PHP או אמצעים מודרניים כמו Guzzle.
- **פרטי יישום:** בזמן שליחת בקשה צריך להיות ער לפרטי אבטחה כמו HTTPS, שימוש ב-headers נכונים וניהול תעודות SSL.

## ראה גם:

- [דוקומנטציה רשמית של PHP על cURL](https://www.php.net/manual/en/book.curl.php)
- [Guzzle, HTTP client for PHP](https://docs.guzzlephp.org/en/stable/)
- [PHP Streams (file_get_contents)](https://www.php.net/manual/en/function.file-get-contents.php)
