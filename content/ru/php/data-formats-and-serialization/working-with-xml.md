---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:50.434467-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0434\u0435\u043B\u0430\u0442\
  \u044C: \u0427\u0442\u0435\u043D\u0438\u0435 XML \u0441 \u043F\u043E\u043C\u043E\
  \u0449\u044C\u044E SimpleXML."
lastmod: '2024-03-13T22:44:45.254924-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u0435\u043D\u0438\u0435 XML \u0441 \u043F\u043E\u043C\u043E\
  \u0449\u044C\u044E SimpleXML."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML"
weight: 40
---

## Как это делать:
Чтение XML с помощью SimpleXML:

```php
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
              <note>
                <to>Tove</to>
                <from>Jani</from>
                <heading>Reminder</heading>
                <body>Не забудь об этом</body>
              </note>';
              
$xml = simplexml_load_string($xmlString);

echo $xml->to;       // Выводит: Tove
echo $xml->from;     // Выводит: Jani
echo $xml->heading;  // Выводит: Reminder
echo $xml->body;     // Выводит: Не забудь об этом
```

Запись XML с помощью DOMDocument:

```php
$dom = new DOMDocument('1.0', 'UTF-8');

$root = $dom->createElement('note');
$dom->appendChild($root);

$to = $dom->createElement('to', 'Tove');
$from = $dom->createElement('from', 'Jani');
$heading = $dom->createElement('heading', 'Reminder');
$body = $dom->createElement('body', 'Не забудь об этом');

$root->appendChild($to);
$root->appendChild($from);
$root->appendChild($heading);
$root->appendChild($body);

echo $dom->saveXML();
```

Пример вывода:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  <body>Не забудь об этом</body>
</note>
```

## Глубже в тему
XML, или Расширяемый язык разметки, остается одним из основных инструментов сериализации данных с момента его рекомендации W3C в 1998 году. Он многословный, понятен человеку и строг в синтаксисе, что делает его надежным выбором для файлов конфигурации, обмена данными и т. д. Однако он был частично затмён JSON для веб-API из-за его простоты и легковесности.

Программисты часто выбирают XML, когда им нужна валидация документов, предоставляемая XML-схемами, или когда они работают в экосистемах, которые уже сильно зависят от него (например, форматы файлов Microsoft Office). Работа с XML в PHP происходит просто с помощью расширения SimpleXML для базовых операций. Для более сложной манипуляции DOMDocument предоставляет набор мощных функций, позволяющих управлять такими аспектами, как обработка пространства имен и валидация схемы.

## Смотрите также
- [PHP: SimpleXML](https://www.php.net/manual/ru/book.simplexml.php)
- [PHP: DOMDocument](https://www.php.net/manual/ru/class.domdocument.php)
- [W3Schools: PHP XML Parsers](https://www.w3schools.com/php/php_xml_parsers.asp)
- [W3C XML Schema](https://www.w3.org/XML/Schema)
