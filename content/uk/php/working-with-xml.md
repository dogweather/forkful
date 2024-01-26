---
title:                "Робота з XML"
date:                  2024-01-26T04:34:49.785379-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/working-with-xml.md"
---

{{< edit_this_page >}}

## Що і чому?
XML — це мова розмітки, що використовується для зберігання та передачі даних. Програмісти працюють з XML для забезпечення взаємодії між додатками та системами — думайте про обмін даними та налаштування конфігурації.

## Як це зробити:
Читання XML за допомогою SimpleXML:

```php
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
              <note>
                <to>Tove</to>
                <from>Jani</from>
                <heading>Reminder</heading>
                <body>Не забудьте про це</body>
              </note>';
              
$xml = simplexml_load_string($xmlString);

echo $xml->to;       // Виведення: Tove
echo $xml->from;     // Виведення: Jani
echo $xml->heading;  // Виведення: Reminder
echo $xml->body;     // Виведення: Не забудьте про це
```

Запис XML за допомогою DOMDocument:

```php
$dom = new DOMDocument('1.0', 'UTF-8');

$root = $dom->createElement('note');
$dom->appendChild($root);

$to = $dom->createElement('to', 'Tove');
$from = $dom->createElement('from', 'Jani');
$heading = $dom->createElement('heading', 'Reminder');
$body = $dom->createElement('body', 'Не забудьте про це');

$root->appendChild($to);
$root->appendChild($from);
$root->appendChild($heading);
$root->appendChild($body);

echo $dom->saveXML();
```

Приклад виводу:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  <body>Не забудьте про це</body>
</note>
```

## Поглиблений аналіз
XML або розширювана мова розмітки стала невід'ємною частиною серіалізації даних з моменту її рекомендації W3C у 1998 році. Вона многослівна, легка для читання людиною і строга у синтаксисі, що робить її надійним вибором для файлів конфігурації, обміну даними та більшого. Втім, JSON частково затьмарив її для веб-API через свою простоту і легковаговість.

Програмісти часто вибирають XML, коли їм потрібна валідація документів, яку надають XML схеми, або коли працюють в екосистемах, що вже значною мірою на ньому базуються (як, наприклад, формати файлів Microsoft Office). Робота з XML у PHP є простою завдяки розширенню SimpleXML для базових операцій. Для більш складної маніпуляції DOMDocument надає надійний набір функцій, які дозволяють мати більший контроль, таких як обробка просторів імен і валідація схем.

## Дивіться також
- [PHP: SimpleXML](https://www.php.net/manual/en/book.simplexml.php)
- [PHP: DOMDocument](https://www.php.net/manual/en/class.domdocument.php)
- [W3Schools: PHP XML Parsers](https://www.w3schools.com/php/php_xml_parsers.asp)
- [W3C XML Schema](https://www.w3.org/XML/Schema)