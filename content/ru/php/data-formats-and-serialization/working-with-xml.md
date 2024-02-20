---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:50.434467-07:00
description: "XML \u2014 \u044D\u0442\u043E \u044F\u0437\u044B\u043A \u0440\u0430\u0437\
  \u043C\u0435\u0442\u043A\u0438, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u0435\u043C\u044B\u0439 \u0434\u043B\u044F \u0445\u0440\u0430\u043D\u0435\u043D\
  \u0438\u044F \u0438 \u043F\u0435\u0440\u0435\u0434\u0430\u0447\u0438 \u0434\u0430\
  \u043D\u043D\u044B\u0445. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\
  \u0441\u0442\u044B \u0440\u0430\u0431\u043E\u0442\u0430\u044E\u0442 \u0441 XML,\
  \ \u0447\u0442\u043E\u0431\u044B \u043E\u0431\u0435\u0441\u043F\u0435\u0447\u0438\
  \u0442\u044C \u0432\u0437\u0430\u0438\u043C\u043E\u0434\u0435\u0439\u0441\u0442\u0432\
  \u0438\u0435 \u043C\u0435\u0436\u0434\u0443 \u043F\u0440\u0438\u043B\u043E\u0436\
  \u0435\u043D\u0438\u044F\u043C\u0438 \u0438\u2026"
lastmod: 2024-02-19 22:05:04.217512
model: gpt-4-0125-preview
summary: "XML \u2014 \u044D\u0442\u043E \u044F\u0437\u044B\u043A \u0440\u0430\u0437\
  \u043C\u0435\u0442\u043A\u0438, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u0435\u043C\u044B\u0439 \u0434\u043B\u044F \u0445\u0440\u0430\u043D\u0435\u043D\
  \u0438\u044F \u0438 \u043F\u0435\u0440\u0435\u0434\u0430\u0447\u0438 \u0434\u0430\
  \u043D\u043D\u044B\u0445. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\
  \u0441\u0442\u044B \u0440\u0430\u0431\u043E\u0442\u0430\u044E\u0442 \u0441 XML,\
  \ \u0447\u0442\u043E\u0431\u044B \u043E\u0431\u0435\u0441\u043F\u0435\u0447\u0438\
  \u0442\u044C \u0432\u0437\u0430\u0438\u043C\u043E\u0434\u0435\u0439\u0441\u0442\u0432\
  \u0438\u0435 \u043C\u0435\u0436\u0434\u0443 \u043F\u0440\u0438\u043B\u043E\u0436\
  \u0435\u043D\u0438\u044F\u043C\u0438 \u0438\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML"
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
