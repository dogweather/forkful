---
title:                "Работа с XML"
date:                  2024-01-29T00:05:57.320174-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/clojure/working-with-xml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
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
