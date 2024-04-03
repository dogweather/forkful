---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:54.010658-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 PHP \u0435\u0441\u0442\u044C \u043D\u0435\u0441\u043A\u043E\
  \u043B\u044C\u043A\u043E \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\u044B\u0445\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A \u0434\u043B\u044F \u0440\
  \u0430\u0437\u0431\u043E\u0440\u0430 HTML, \u0442\u0430\u043A\u0438\u0445 \u043A\
  \u0430\u043A DOMDocument. \u0412\u043E\u0442 \u043F\u0440\u043E\u0441\u0442\u043E\
  \u0439 \u043F\u0440\u0438\u043C\u0435\u0440 \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u043E\u0432\u0430\u043D\u0438\u044F."
lastmod: '2024-03-13T22:44:45.206144-06:00'
model: gpt-4-0125-preview
summary: "\u0412 PHP \u0435\u0441\u0442\u044C \u043D\u0435\u0441\u043A\u043E\u043B\
  \u044C\u043A\u043E \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\u044B\u0445\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A \u0434\u043B\u044F \u0440\
  \u0430\u0437\u0431\u043E\u0440\u0430 HTML, \u0442\u0430\u043A\u0438\u0445 \u043A\
  \u0430\u043A DOMDocument."
title: "\u0420\u0430\u0437\u0431\u043E\u0440 HTML"
weight: 43
---

## Как это сделать:
В PHP есть несколько встроенных библиотек для разбора HTML, таких как DOMDocument. Вот простой пример использования:

```php
<?php
$htmlString = '<!DOCTYPE html><html><body><h1>Привет, мир!</h1></body></html>';
$dom = new DOMDocument();
@$dom->loadHTML($htmlString); // '@' подавляет предупреждения, вызванные невалидными HTML-структурами
$h1Tags = $dom->getElementsByTagName('h1');

foreach ($h1Tags as $tag) {
    echo $tag->nodeValue; // Вывод: Привет, мир!
}
?>
```

Этот скрипт выводит: `Привет, мир!`

## Подробнее
В начале эпохи веб-разработки мы получали HTML с помощью regex и ad-hoc решений, но это было неудобно. Появление `DOMDocument` и `SimpleXMLElement`, начиная с PHP 5, позволило корректно разбирать HTML и XML. Они позволяют навигировать и манипулировать HTML как древовидной структурой.

В наши дни, пока `DOMDocument` является вашим основным инструментом для внутреннего разбора, альтернативы вроде `SimpleHTMLDom` и `phpQuery` предоставляют дополнительные удобства и могут быть более понятны для тех, кто пришел из мира JavaScript/jQuery.

Внутри, `DOMDocument` преобразует HTML в DOM-дерево, упрощая доступ к конкретным элементам, изменение атрибутов, и даже модификацию документа "на лету". Одна из крутых особенностей `DOMDocument` — его терпимость к плохому HTML, исправление его и позволяющее работать с реальными веб-страницами, которые не всегда идеально форматированы.

## Смотрите также
- [DOMDocument на PHP.net](https://www.php.net/manual/ru/class.domdocument.php)
- [SimpleXML для выполнения базовых задач XML](https://www.php.net/manual/ru/book.simplexml.php)
- [Проект simplehtmldom на SourceForge](https://sourceforge.net/projects/simplehtmldom/)
- [Репозиторий phpQuery на GitHub](https://github.com/punkave/phpQuery)
