---
title:                "Работа с XML"
date:                  2024-01-29T00:05:26.358092-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с XML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/gleam/working-with-xml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Работа с XML включает в себя анализ, манипуляции и создание XML-документов, которые используются для обмена данными благодаря их структурированному и широко распространенному формату. Программисты работают с XML для взаимодействия с бесчисленным количеством систем, где XML является универсальным языком данных.

## Как это сделать:
Gleam изначально не поддерживает XML, поэтому мы будем использовать внешнюю библиотеку типа `gleam_xml`. Сначала добавьте ее в ваш `gleam.toml`:

```toml
[dependencies]
gleam_xml = "~> 1.0"
```

Теперь разберем и создадим XML:

```rust
import gleam/xml

// Разбор XML
let doc = xml.parse("<note><to>Tove</to><from>Jani</from></note>")?

// Создание XML
let node = xml.Element(
  "note",
  [],
  [
    xml.Element("to", [], [xml.Text("Tove")]),
    xml.Element("from", [], [xml.Text("Jani")]),
  ]
)
let xml_string = xml.render(node)
```

Пример вывода для `xml.render(node)`:

```xml
<note><to>Tove</to><from>Jani</from></note>
```

## Глубокое погружение
XML означает eXtensible Markup Language, спецификация от W3C как сестра HTML. Существует с конца 90-х. Для Gleam работа с XML кажется немного шагом назад во времени. JSON и Protocol Buffers более модные, но широкое использование XML в наследственных системах и отдельных отраслях означает, что он все еще актуален.

Существуют альтернативы вроде `xmerl` в экосистеме Erlang; однако, библиотека `gleam_xml` предлагает более идиоматический подход для пользователей Gleam. Она построена на основе существующих библиотек Erlang, но предоставляет API, удобный для Gleam. Подход Gleam к XML стремится к простоте и безопасности, сокращая шаблонный код и акцентируя внимание на типовой безопасности.

С точки зрения реализации, библиотеки XML, включая `gleam_xml`, обычно предоставляют структуры похожие на DOM. Это включает в себя узлы, атрибуты и вложенные элементы, используя возможности сопоставления с образцом и модели конкуренции Erlang для обработки потенциально больших и сложных документов.

## Смотрите также
- Библиотека `gleam_xml` на Hex: [https://hex.pm/packages/gleam_xml](https://hex.pm/packages/gleam_xml)
- Официальный стандарт XML от W3C: [https://www.w3.org/XML/](https://www.w3.org/XML/)
- Подробный учебник по XML: [https://www.w3schools.com/xml/](https://www.w3schools.com/xml/)
- Документация по `xmerl` Erlang для обработки XML: [http://erlang.org/doc/apps/xmerl/](http://erlang.org/doc/apps/xmerl/)
