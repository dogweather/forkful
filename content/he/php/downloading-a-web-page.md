---
title:                "PHP: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

שלום לכולם!

## למה

אז, אתם רוצים לדרוס דף אינטרנט ללא הצורך להיכנס בעזרת הדפדפן? אין בעיה! דרסת דף אינטרנט היא פעולה פשוטה שכיף לנסות והיא יכולה להיות מאוד שווה במצבים שבהם יש צורך בדירוס כמה פעמים את הדף הזהה ולא רק לחפש אותו כל פעם מחדש ולהיתקע במסכי הלוגין.

## איך לעשות זאת

לפניכם כמה דרכים שונות לדרוס דף אינטרנט בעזרת PHP:

```php
<?php

//דוגמה ראשונה: השתמשו בפונקציית file_get_contents כדי לקבל את תוכן הדף הרצוי
$page = file_get_contents("https://www.example.com");
echo $page; // ימצא את כל הקוד ה-HTML של הדף וידפיס אותו למסך

//דוגמה שנייה: השתמשו בפונקציית curl לדרוס דף אינטרנט ולקבל את תוכן הדף הרצוי
$ch = curl_init("https://www.example.com");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$page = curl_exec($ch);
echo $page; // ימצא את כל הקוד ה-HTML של הדף וידפיס אותו למסך

//דוגמה שלישית: השתמשו בספריית Guzzle כדי לדרוס דף אינטרנט ולקבל את תוכן הדף הרצוי
require 'vendor/autoload.php';
use GuzzleHttp\Client;
$client = new Client();
$response = $client->request('GET', 'https://www.example.com');
$page = $response->getBody()->getContents();
echo $page; // ימצא את כל הקוד ה-HTML של הדף וידפיס אותו למסך
```

## ייעולים

כעת שאתם יודעים כיצד לדרוס דף אינטרנט בעזרת PHP, אפשר לשפר את התוצאות שאתם מקבלים. לדוגמה, אם אתם דורסים דף אינטרנט רק כדי לאתר את חלק מסוים של הקוד ה-HTML, אתם יכולים להשתמש בפעולות ניתוח של PHP כדי למצוא מהר יותר את המידע הרל