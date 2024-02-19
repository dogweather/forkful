---
aliases:
- /ru/clojure/working-with-xml/
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:57.320174-07:00
description: "XML - \u044D\u0442\u043E \u044F\u0437\u044B\u043A \u0440\u0430\u0437\
  \u043C\u0435\u0442\u043A\u0438 \u0434\u043B\u044F \u043A\u043E\u0434\u0438\u0440\
  \u043E\u0432\u0430\u043D\u0438\u044F \u0434\u043E\u043A\u0443\u043C\u0435\u043D\u0442\
  \u043E\u0432 \u0442\u0430\u043A\u0438\u043C \u043E\u0431\u0440\u0430\u0437\u043E\
  \u043C, \u0447\u0442\u043E\u0431\u044B \u043E\u043D\u0438 \u0431\u044B\u043B\u0438\
  \ \u0447\u0438\u0442\u0430\u0435\u043C\u044B \u043A\u0430\u043A \u0434\u043B\u044F\
  \ \u0447\u0435\u043B\u043E\u0432\u0435\u043A\u0430, \u0442\u0430\u043A \u0438 \u0434\
  \u043B\u044F \u043C\u0430\u0448\u0438\u043D\u044B. \u041E\u043D \u043A\u043B\u044E\
  \u0447\u0435\u0432\u043E\u0439 \u0432 \u0432\u0435\u0431-\u0441\u0435\u0440\u0432\
  \u0438\u0441\u0430\u0445,\u2026"
lastmod: 2024-02-18 23:08:56.611655
model: gpt-4-0125-preview
summary: "XML - \u044D\u0442\u043E \u044F\u0437\u044B\u043A \u0440\u0430\u0437\u043C\
  \u0435\u0442\u043A\u0438 \u0434\u043B\u044F \u043A\u043E\u0434\u0438\u0440\u043E\
  \u0432\u0430\u043D\u0438\u044F \u0434\u043E\u043A\u0443\u043C\u0435\u043D\u0442\u043E\
  \u0432 \u0442\u0430\u043A\u0438\u043C \u043E\u0431\u0440\u0430\u0437\u043E\u043C\
  , \u0447\u0442\u043E\u0431\u044B \u043E\u043D\u0438 \u0431\u044B\u043B\u0438 \u0447\
  \u0438\u0442\u0430\u0435\u043C\u044B \u043A\u0430\u043A \u0434\u043B\u044F \u0447\
  \u0435\u043B\u043E\u0432\u0435\u043A\u0430, \u0442\u0430\u043A \u0438 \u0434\u043B\
  \u044F \u043C\u0430\u0448\u0438\u043D\u044B. \u041E\u043D \u043A\u043B\u044E\u0447\
  \u0435\u0432\u043E\u0439 \u0432 \u0432\u0435\u0431-\u0441\u0435\u0440\u0432\u0438\
  \u0441\u0430\u0445,\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML"
---

{{< edit_this_page >}}

## Что и Зачем?
XML - это язык разметки для кодирования документов таким образом, чтобы они были читаемы как для человека, так и для машины. Он ключевой в веб-сервисах, файлах конфигурации и обмене данными, поскольку переносит данные в структурированном, иерархическом формате.

## Как это сделать:
Clojure предлагает библиотеку `clojure.data.xml` для парсинга и вывода XML. Давайте для начала распарсим немного XML:

```clojure
(require '[clojure.data.xml :as xml])

(let [content "<root><foo>bar</foo><foo>baz</foo></root>"
      parsed (xml/parse-str content)] ; Парсинг строки XML
  (println parsed))
```
Вывод:
```
Element{:tag :root, :attrs {}, :content (Element{:tag :foo, :attrs {}, :content ("bar")} Element{:tag :foo, :attrs {}, :content ("baz")})}
```

Для создания XML из структур Clojure:

```clojure
(def my-xml (xml/element :root {}
                          (xml/element :foo {} "bar")
                          (xml/element :foo {} "baz")))

(println (xml/emit-str my-xml))
```
Вывод:
```
<root><foo>bar</foo><foo>baz</foo></root>
```

## Подробнее
XML появился в конце 90-х как упрощенный подмножество SGML, предназначенный для данных в вебе. Его использование взлетело с такими технологиями, как SOAP и XHTML, но ему нашлась конкуренция в лице JSON, который предпочитают за его легкость и простоту.

Подход Clojure к XML сохраняет его функциональность и ориентацию на данные, оставаясь верным этике языка. `clojure.data.xml` - это всего лишь один из вариантов; у вас есть `clojure.xml` для базовых потребностей, и для взаимодействия с Java вы можете использовать тяжеловесы типа JAXB или DOM4J.

Имейте в виду, что производительность и потребление памяти при работе с очень большими XML документами может быть значительным. Потоковые парсеры, такие как StAX, могут помочь, но вам придется обратиться к Java-миру для их использования.

## Смотрите также
- [clojure.data.xml GitHub](https://github.com/clojure/data.xml)
- [Java API для обработки XML (JAXP)](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [StAX](https://docs.oracle.com/javase/tutorial/jaxp/stax/index.html)
