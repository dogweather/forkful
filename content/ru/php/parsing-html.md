---
title:                "Разбор HTML"
aliases:
- ru/php/parsing-html.md
date:                  2024-01-28T23:59:54.010658-07:00
model:                 gpt-4-0125-preview
simple_title:         "Разбор HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/php/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Разбор HTML представляет собой обработку и анализ структуры и содержания HTML-кода. Программисты делают это для сбора данных с веб-сайтов, манипуляции или извлечения данных, автоматизации тестирования или интеграции веб-контента в приложения.

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
