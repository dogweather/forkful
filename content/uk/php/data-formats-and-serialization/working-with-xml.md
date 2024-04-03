---
date: 2024-01-26 04:34:49.785379-07:00
description: "XML \u2014 \u0446\u0435 \u043C\u043E\u0432\u0430 \u0440\u043E\u0437\u043C\
  \u0456\u0442\u043A\u0438, \u0449\u043E \u0432\u0438\u043A\u043E\u0440\u0438\u0441\
  \u0442\u043E\u0432\u0443\u0454\u0442\u044C\u0441\u044F \u0434\u043B\u044F \u0437\
  \u0431\u0435\u0440\u0456\u0433\u0430\u043D\u043D\u044F \u0442\u0430 \u043F\u0435\
  \u0440\u0435\u0434\u0430\u0447\u0456 \u0434\u0430\u043D\u0438\u0445. \u041F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u043F\u0440\u0430\u0446\u044E\
  \u044E\u0442\u044C \u0437 XML \u0434\u043B\u044F \u0437\u0430\u0431\u0435\u0437\u043F\
  \u0435\u0447\u0435\u043D\u043D\u044F \u0432\u0437\u0430\u0454\u043C\u043E\u0434\u0456\
  \u0457 \u043C\u0456\u0436 \u0434\u043E\u0434\u0430\u0442\u043A\u0430\u043C\u0438\
  \ \u0442\u0430\u2026"
lastmod: '2024-03-13T22:44:49.472055-06:00'
model: gpt-4-0125-preview
summary: "XML \u2014 \u0446\u0435 \u043C\u043E\u0432\u0430 \u0440\u043E\u0437\u043C\
  \u0456\u0442\u043A\u0438, \u0449\u043E \u0432\u0438\u043A\u043E\u0440\u0438\u0441\
  \u0442\u043E\u0432\u0443\u0454\u0442\u044C\u0441\u044F \u0434\u043B\u044F \u0437\
  \u0431\u0435\u0440\u0456\u0433\u0430\u043D\u043D\u044F \u0442\u0430 \u043F\u0435\
  \u0440\u0435\u0434\u0430\u0447\u0456 \u0434\u0430\u043D\u0438\u0445."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

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
