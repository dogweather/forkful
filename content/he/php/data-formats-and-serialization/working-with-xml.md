---
title:                "עבודה עם XML"
aliases:
- /he/php/working-with-xml/
date:                  2024-01-26T04:34:24.619293-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/working-with-xml.md"
---

{{< edit_this_page >}}

## מה ולמה?
XML הוא שפת סימון המשמשת לאחסון ושינוע נתונים. מתכנתים עובדים עם XML כדי לאפשר אינטרופרביליות בין יישומים ומערכות - חשבו על חליפין של נתונים והגדרות תצורה.

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
