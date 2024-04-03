---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:57.320174-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Clojure \u043F\u0440\u0435\u0434\u043B\u0430\u0433\u0430\u0435\u0442\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443 `clojure.data.xml`\
  \ \u0434\u043B\u044F \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0430 \u0438 \u0432\
  \u044B\u0432\u043E\u0434\u0430 XML. \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0434\
  \u043B\u044F \u043D\u0430\u0447\u0430\u043B\u0430 \u0440\u0430\u0441\u043F\u0430\
  \u0440\u0441\u0438\u043C \u043D\u0435\u043C\u043D\u043E\u0433\u043E XML."
lastmod: '2024-03-13T22:44:44.393991-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u043F\u0440\u0435\u0434\u043B\u0430\u0433\u0430\u0435\u0442 \u0431\
  \u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443 `clojure.data.xml` \u0434\
  \u043B\u044F \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0430 \u0438 \u0432\u044B\
  \u0432\u043E\u0434\u0430 XML."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML"
weight: 40
---

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
