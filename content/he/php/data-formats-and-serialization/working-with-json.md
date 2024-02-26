---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:08.766771-07:00
description: "JSON, \u05D0\u05D5 JavaScript Object Notation, \u05D4\u05D5\u05D0 \u05E4\
  \u05D5\u05E8\u05DE\u05D8 \u05DC\u05D4\u05D7\u05DC\u05E4\u05EA \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05E7\u05DC\u05D9\u05DC \u05D4\u05DF \u05DC\u05E7\u05E8\u05D9\
  \u05D0\u05D4 \u05D5\u05D4\u05DF \u05DC\u05DB\u05EA\u05D9\u05D1\u05D4 \u05E2\u05DC\
  \ \u05D9\u05D3\u05D9 \u05D1\u05E0\u05D9 \u05D0\u05D3\u05DD, \u05D5\u05D2\u05DD \u05E7\
  \u05DC \u05DC\u05E0\u05D9\u05EA\u05D5\u05D7 \u05D5\u05D9\u05D9\u05E6\u05D5\u05E8\
  \ \u05E2\u05DC \u05D9\u05D3\u05D9 \u05DE\u05DB\u05D5\u05E0\u05D5\u05EA. \u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD\u2026"
lastmod: '2024-02-25T18:49:37.756410-07:00'
model: gpt-4-0125-preview
summary: "JSON, \u05D0\u05D5 JavaScript Object Notation, \u05D4\u05D5\u05D0 \u05E4\
  \u05D5\u05E8\u05DE\u05D8 \u05DC\u05D4\u05D7\u05DC\u05E4\u05EA \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05E7\u05DC\u05D9\u05DC \u05D4\u05DF \u05DC\u05E7\u05E8\u05D9\
  \u05D0\u05D4 \u05D5\u05D4\u05DF \u05DC\u05DB\u05EA\u05D9\u05D1\u05D4 \u05E2\u05DC\
  \ \u05D9\u05D3\u05D9 \u05D1\u05E0\u05D9 \u05D0\u05D3\u05DD, \u05D5\u05D2\u05DD \u05E7\
  \u05DC \u05DC\u05E0\u05D9\u05EA\u05D5\u05D7 \u05D5\u05D9\u05D9\u05E6\u05D5\u05E8\
  \ \u05E2\u05DC \u05D9\u05D3\u05D9 \u05DE\u05DB\u05D5\u05E0\u05D5\u05EA. \u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON"
---

{{< edit_this_page >}}

## מה ולמה?
JSON, או JavaScript Object Notation, הוא פורמט להחלפת נתונים קליל הן לקריאה והן לכתיבה על ידי בני אדם, וגם קל לניתוח וייצור על ידי מכונות. תכנתים לעיתים קרובות עובדים עם JSON כדי להחליף נתונים בין שרתים ויישומי אינטרנט בזכות פשטותו ועצמאיותו משפה, דבר ההופך אותו לאבן פינה בפיתוח האינטרנט המודרני וב-APIs.

## איך לעשות:
עבודה עם JSON ב-PHP פשוטה הודות לפונקציות המובנות `json_encode()` ו-`json_decode()`. להלן דוגמאות המציגות איך להמיר מערך PHP למחרוזת JSON, ולהיפך:

### קידוד מערך PHP למחרוזת JSON
```php
// הגדרת מערך אסוציאטיבי
$data = [
    "name" => "John Doe",
    "age" => 30,
    "email" => "john.doe@example.com"
];

// המרת מערך ה-PHP למחרוזת JSON
$jsonString = json_encode($data);

// הדפסת המחרוזת JSON
echo $jsonString;
```
**תוצאה לדוגמה:**
```json
{"name":"John Doe","age":30,"email":"john.doe@example.com"}
```

### פענוח מחרוזת JSON למערך PHP
```php
// מחרוזת JSON
$jsonString = '{"name":"John Doe","age":30,"email":"john.doe@example.com"}';

// המרת המחרוזת JSON למערך PHP
$data = json_decode($jsonString, true);

// הדפסת המערך PHP
print_r($data);
```
**תוצאה לדוגמה:**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```

### עבודה עם ספריית צד שלישי: GuzzleHttp
עבור טיפול מורכב ב-JSON ובקשות אינטרנט, אחת הספריות הפופולריות ב-PHP היא GuzzleHttp. היא מפשטת בקשות HTTP ועובדת בקלות עם נתוני JSON.

**התקנה דרך Composer:**
```
composer require guzzlehttp/guzzle
```

**דוגמת בקשה:**
```php
require 'vendor/autoload.php';

use GuzzleHttp\Client;

$client = new Client();

// שליחת בקשה ל-API שמחזיר JSON
$response = $client->request('GET', 'https://api.example.com/data', [
    'headers' => [
        'Accept' => 'application/json',
    ],
]);

// פענוח התגובה JSON למערך PHP
$data = json_decode($response->getBody(), true);

// הדפסת הנתונים
print_r($data);
```

**בהנחה שה-API מחזיר נתוני JSON דומים:**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```
זה מציג את קלות השימוש ב-PHP למניפולציה של JSON, גם עם פונקציות מקוריות וגם עם ספריות רבות עוצמה כמו GuzzleHttp למשימות מורכבות יותר.
