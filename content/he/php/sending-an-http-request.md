---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP היא בסיס לתכנות שרתי web: היא אומרת לשרת ווב תתחיל שיחה. תכניתיים עושים זאת לשלוט ברשת, לשלוף מידע או לשלוח מידע לשרת אחר.

## איך לעשות:
PHP מציע מספר שיטות לשליחת בקשות HTTP. לדוגמא:

```PHP
<?php
$url = 'http://example.com';
$ch = curl_init($url);

curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$response = curl_exec($ch);

// print the response:
echo "Response: " . $response;
curl_close($ch);
?>
```

פלט דוגמה:

```
Response: <html><body>...</body></html>
```

## צלילה עמוקה:
שליחת בקשות HTTP לא התקיימה מאז שהפרוטוקול HTTP נוצר ב1991. שיטות אלטרנטיביות לשליחת בקשות HTTP ב-PHP כוללות את השימוש ב- `stream_context_create()` או בספריות כמו GuzzleHTTP. בנוסף, קיימות הרבה טכניקות לשליחת בקשות HTTP כמו: GET, POST, ו-option.

## ראה גם:
1. [מדריך PHP: עזר שליחת בקשת HTTP](https://www.php.net/manual/en/book.curl.php)
2. [GuzzleHTTP, ספרייה המבצעת בקשות HTTP](http://docs.guzzlephp.org/en/stable/) 
3. [שימוש בפונקציה stream_context_create()](https://www.php.net/manual/en/function.stream-context-create.php)