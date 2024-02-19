---
aliases:
- /he/php/sending-an-http-request-with-basic-authentication/
date: 2024-01-20 18:02:45.444613-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05D4\u05D9\u05D0\
  \ \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05DE\u05E9\u05EA\u05DE\u05E9\
  \u05D9\u05DD \u05D1\u05E9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9 \u05D5\u05E1\u05D9\
  \u05E1\u05DE\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D1\u05DC \u05D2\u05D9\u05E9\
  \u05D4 \u05DC\u05DE\u05E9\u05D0\u05D1 \u05D1\u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\
  \u05D8. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D1\u05D8\u05D9\u05D7\
  \ \u05E9\u05E8\u05E7 \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05DE\u05D5\u05E8\
  \u05E9\u05D9\u05DD\u2026"
lastmod: 2024-02-18 23:08:52.931438
model: gpt-4-1106-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05D4\u05D9\u05D0\
  \ \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05DE\u05E9\u05EA\u05DE\u05E9\
  \u05D9\u05DD \u05D1\u05E9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9 \u05D5\u05E1\u05D9\
  \u05E1\u05DE\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D1\u05DC \u05D2\u05D9\u05E9\
  \u05D4 \u05DC\u05DE\u05E9\u05D0\u05D1 \u05D1\u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\
  \u05D8. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D1\u05D8\u05D9\u05D7\
  \ \u05E9\u05E8\u05E7 \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05DE\u05D5\u05E8\
  \u05E9\u05D9\u05DD\u2026"
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי היא פעולה שבה משתמשים בשם משתמש וסיסמה כדי לקבל גישה למשאב באינטרנט. תכניתנים עושים זאת כדי להבטיח שרק משתמשים מורשים יכולים לראות או לערוך מידע רגיש.

## איך לעשות:
```PHP
<?php
$url = 'http://example.com/data';
$username = 'your_username';
$password = 'your_password';

$context = stream_context_create([
    'http' => [
        'header' => 'Authorization: Basic ' . base64_encode("$username:$password")
    ]
]);

$result = file_get_contents($url, false, $context);

if ($result !== false) {
    echo $result;
} else {
    echo 'Authentication failed or other error.';
}
?>
```

פלט לדוגמא:
```PHP
{
  "id": 123,
  "content": "Data accessible only with valid credentials."
}
```

## עיון מעמיק:
בשנים הראשונות של האינטרנט, אימות בסיסי דרך HTTP נפוץ מאוד. למרות שהוא לא מציע אבטחה גבוהה, זה מספק שכבת הגנה ראשונית. אלטרנטיבות כמו אימות Digest, OAuth, ו-API Keys הופכות לנפוצות יותר בשל רמות אבטחה גבוהות יותר. עם זאת, אימות בסיסי נשאר פופולרי למקרים של שימוש פנימי או במערכות עם סיכון אבטחה נמוך.

בעת שליחת בקשה עם אימות בסיסי ב-PHP, משתמשים בפונקציה `base64_encode` כדי להעביר את שם המשתמש והסיסמה בצורה מקודדת. חשוב לזכור שפרטים אלו נשלחים בגלוי בתוך הבקשה, לכן מומלץ לשימוש רק עם HTTPS.

עבור קוד PHP מתקדם יותר, שימוש ב-cURL או בספריות התקשורת מומלץ. זה נותן שליטה טובה יותר על התהליך ותמיכה באפשרויות אימות מתקדמות.

## ראה גם:
- [HTTP authentication with PHP](https://www.php.net/manual/en/features.http-auth.php)
- [cURL module for PHP](https://www.php.net/manual/en/book.curl.php)
- [HTTPS and PHP](https://www.php.net/manual/en/context.ssl.php)
