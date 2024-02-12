---
title:                "Работа с XML"
aliases: - /ru/php/working-with-xml.md
date:                  2024-01-29T00:04:50.434467-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/php/working-with-xml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
XML — это язык разметки, используемый для хранения и передачи данных. Программисты работают с XML, чтобы обеспечить взаимодействие между приложениями и системами — думайте о обмене данными и настройках конфигурации.

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
