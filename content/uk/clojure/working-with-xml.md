---
title:                "Робота з XML"
date:                  2024-01-26T04:29:32.926515-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/working-with-xml.md"
---

{{< edit_this_page >}}

## Що та Чому?
XML - це мова розмітки для кодування документів у спосіб, який є зрозумілим як для людини, так і для машини. Він ключовий у веб-сервісах, файлах конфігурації та обміні даними, оскільки передає дані у структурованому, ієрархічному форматі.

## Як:
Clojure пропонує бібліотеку `clojure.data.xml` для розбору та виведення XML. Спочатку давайте розберемо деякий XML:

```clojure
(require '[clojure.data.xml :as xml])

(let [content "<root><foo>bar</foo><foo>baz</foo></root>"
      parsed (xml/parse-str content)] ; Розбір рядка XML
  (println parsed))
```
Вивід:
```
Element{:tag :root, :attrs {}, :content (Element{:tag :foo, :attrs {}, :content ("bar")} Element{:tag :foo, :attrs {}, :content ("baz")})}
```

Щоб згенерувати XML зі структур Clojure:

```clojure
(def my-xml (xml/element :root {}
                          (xml/element :foo {} "bar")
                          (xml/element :foo {} "baz")))

(println (xml/emit-str my-xml))
```
Вивід:
```
<root><foo>bar</foo><foo>baz</foo></root>
```

## Поглиблений Огляд
XML з'явився ще в кінці 90-х років як спрощений підмножина SGML, призначений для веб-даних. Його використання стрімко зросло з появою таких технологій, як SOAP та XHTML, але з'явилася певна конкуренція від JSON, який віддають перевагу за його легкість і простоту.

Підхід Clojure до XML залишається функціональним і орієнтованим на дані, відповідно до етосу мови. `clojure.data.xml` - це лише один із варіантів; ви маєте `clojure.xml` для базових потреб, а для інтероперабельності з Java ви можете використовувати потужні інструменти, такі як JAXB або DOM4J.

Майте на увазі, що продуктивність та навантаження на пам'ять при роботі з дуже великими XML документами можуть бути значними. Потокові парсери, як-от StAX, можуть допомогти, але для них вам потрібно буде зануритися у світ Java.

## Дивіться Також
- [clojure.data.xml GitHub](https://github.com/clojure/data.xml)
- [Java API для обробки XML (JAXP)](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [StAX](https://docs.oracle.com/javase/tutorial/jaxp/stax/index.html)