---
date: 2024-01-26 04:34:24.619293-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05E7\u05E8\u05D9\u05D0\u05EA XML \u05D1\
  \u05D0\u05DE\u05E6\u05E2\u05D5\u05EA SimpleXML."
lastmod: '2024-03-13T22:44:39.521112-06:00'
model: gpt-4-0125-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA XML \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA\
  \ SimpleXML."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML"
weight: 40
---

## איך ל:
קריאת XML באמצעות SimpleXML:

```php
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
              <note>
                <to>Tove</to>
                <from>Jani</from>
                <heading>תזכורת</heading>
                <body>אל תשכח את זה</body>
              </note>';
              
$xml = simplexml_load_string($xmlString);

echo $xml->to;       // פלט: Tove
echo $xml->from;     // פלט: Jani
echo $xml->heading;  // פלט: תזכורת
echo $xml->body;     // פלט: אל תשכח את זה
```

כתיבת XML באמצעות DOMDocument:

```php
$dom = new DOMDocument('1.0', 'UTF-8');

$root = $dom->createElement('note');
$dom->appendChild($root);

$to = $dom->createElement('to', 'Tove');
$from = $dom->createElement('from', 'Jani');
$heading = $dom->createElement('heading', 'תזכורת');
$body = $dom->createElement('body', 'אל תשכח את זה');

$root->appendChild($to);
$root->appendChild($from);
$root->appendChild($heading);
$root->appendChild($body);

echo $dom->saveXML();
```

דוגמת פלט:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>תזכורת</heading>
  <body>אל תשכח את זה</body>
</note>
```

## עיון מעמיק
XML, או שפת הסימון הניתנת להרחבה, הייתה עמוד תווך בסריאליזציה של נתונים מאז המלצת ה-W3C ב-1998. היא מפורטת, קריאה לאדם וקפדנית בתחביר שלה, מה שהופך אותה לבחירה אמינה עבור קבצי תצורה, חליפין נתונים, ועוד. עם זאת, היא נעקפה חלקית על ידי JSON עבור Web APIs בשל פשטותה וקלות משקלה.

מתכנתים לעיתים קורות בוחרים ב-XML כאשר הם זקוקים לאימות מסמכים המוספק על ידי סכימות XML או כאשר הם עובדים באקוסיסטמות שנשענות עליו במידה רבה (כמו פורמטי קבצים של Microsoft Office). טיפול ב-XML ב-PHP הוא פשוט עם הרחבת SimpleXML לפעולות בסיסיות. למניפולציה מורכבת יותר, DOMDocument מספק סט מתקדם של תכונות המאפשרות שליטה רבה יותר, כגון טיפול במרחבי שמות ואימות סכימה.

## ראה גם
- [PHP: SimpleXML](https://www.php.net/manual/en/book.simplexml.php)
- [PHP: DOMDocument](https://www.php.net/manual/en/class.domdocument.php)
- [W3Schools: PHP XML Parsers](https://www.w3schools.com/php/php_xml_parsers.asp)
- [סכימת XML של W3C](https://www.w3.org/XML/Schema)
