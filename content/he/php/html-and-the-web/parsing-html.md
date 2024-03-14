---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:15.859184-07:00
description: "\u05E0\u05D9\u05EA\u05D5\u05D7 HTML \u05D1-PHP \u05DE\u05E2\u05E8\u05D1\
  \ \u05D7\u05D9\u05DC\u05D5\u05E5 \u05DE\u05D9\u05D3\u05E2 \u05DE\u05E1\u05D5\u05D9\
  \u05DD \u05DE\u05DE\u05E1\u05DE\u05DB\u05D9 HTML. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\u05DD \u05DE\u05E9\u05D9\u05DE\u05D4\
  \ \u05D6\u05D5 \u05DB\u05D3\u05D9 \u05DC\u05D0\u05D5\u05D8\u05DE\u05D8 \u05D0\u05EA\
  \ \u05D7\u05D9\u05DC\u05D5\u05E5 \u05D4\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\
  \u05D2\u05E8\u05D5\u05E3 \u05D0\u05EA\u05E8\u05D9 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8, \u05D0\u05D5 \u05DC\u05E9\u05DC\u05D1 \u05EA\u05D5\u05DB\u05DF \u05DE\
  \u05D3\u05E4\u05D9 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8\u2026"
lastmod: '2024-03-13T22:44:39.477166-06:00'
model: gpt-4-0125-preview
summary: "\u05E0\u05D9\u05EA\u05D5\u05D7 HTML \u05D1-PHP \u05DE\u05E2\u05E8\u05D1\
  \ \u05D7\u05D9\u05DC\u05D5\u05E5 \u05DE\u05D9\u05D3\u05E2 \u05DE\u05E1\u05D5\u05D9\
  \u05DD \u05DE\u05DE\u05E1\u05DE\u05DB\u05D9 HTML. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\u05DD \u05DE\u05E9\u05D9\u05DE\u05D4\
  \ \u05D6\u05D5 \u05DB\u05D3\u05D9 \u05DC\u05D0\u05D5\u05D8\u05DE\u05D8 \u05D0\u05EA\
  \ \u05D7\u05D9\u05DC\u05D5\u05E5 \u05D4\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\
  \u05D2\u05E8\u05D5\u05E3 \u05D0\u05EA\u05E8\u05D9 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8, \u05D0\u05D5 \u05DC\u05E9\u05DC\u05D1 \u05EA\u05D5\u05DB\u05DF \u05DE\
  \u05D3\u05E4\u05D9 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8\u2026"
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML"
---

{{< edit_this_page >}}

## מה ולמה?
ניתוח HTML ב-PHP מערב חילוץ מידע מסוים ממסמכי HTML. מתכנתים מבצעים משימה זו כדי לאוטמט את חילוץ הנתונים, לגרוף אתרי אינטרנט, או לשלב תוכן מדפי אינטרנט שונים בתוך היישומים שלהם, ובכך לשפר את הפונקציונליות ללא התערבות ידנית.

## איך לעשות:
לניתוח HTML, מתכנתי PHP יכולים להשתמש בפונקציות מובנות או להיעזר בספריות חזקות כמו Simple HTML DOM Parser. כאן, נחקור דוגמאות באמצעות ה-`DOMDocument` של PHP וה-Simple HTML DOM Parser.

### באמצעות `DOMDocument`:
המחלקה `DOMDocument` של PHP היא חלק מההרחבה ה-DOM שלו, שמאפשרת ניתוח ומניפולציה של מסמכי HTML ו-XML. הנה דוגמא מהירה איך להשתמש ב-`DOMDocument` כדי למצוא את כל התמונות במסמך HTML:

```php
$html = <<<HTML
<!DOCTYPE html>
<html>
<head>
    <title>דף דוגמה</title>
</head>
<body>
    <img src="image1.jpg" alt="תמונה 1">
    <img src="image2.jpg" alt="תמונה 2">
</body>
</html>
HTML;

$doc = new DOMDocument();
@$doc->loadHTML($html);
$images = $doc->getElementsByTagName('img');

foreach ($images as $img) {
    echo $img->getAttribute('src') . "\n";
}
```

פלט לדוגמה:
```
image1.jpg
image2.jpg
```

### באמצעות Simple HTML DOM Parser:
למשימות מורכבות יותר או תחביר נוח יותר, ייתכן שתעדיף להשתמש בספריה של צד שלישי. Simple HTML DOM Parser הוא בחירה פופולרית, שמספקת ממשק דומה ל-jQuery לניווט ומניפולציה של מבנים HTML. הנה איך להשתמש בו:

ראשית, התקן את הספריה באמצעות Composer:
```
composer require simple-html-dom/simple-html-dom
```

אחר כך, מניפול את ה-HTML למצוא למשל את כל הקישורים:

```php
require_once 'vendor/autoload.php';

use simplehtmldom\HtmlWeb;

$client = new HtmlWeb();
$html = $client->load('http://www.example.com');

foreach($html->find('a') as $element) {
    echo $element->href . "\n";
}
```

קטע הקוד הזה ייקח את תוכן ה-HTML של 'http://www.example.com', ינתח אותו, וידפיס את כל הקישורים. זכור להחליף את 'http://www.example.com' ב-URL האמיתי שאתה רוצה לנתח.

בעזרת שיטות אלו, מפתחי PHP יכולים לנתח תוכן HTML ביעילות, להתאים את חילוץ הנתונים לצרכים שלהם, או לשלב תוכן אינטרנטי חיצוני בפרויקטים שלהם בחלקות.
