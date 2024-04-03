---
date: 2024-01-26 04:29:32.926515-07:00
description: "\u042F\u043A: Clojure \u043F\u0440\u043E\u043F\u043E\u043D\u0443\u0454\
  \ \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0443 `clojure.data.xml`\
  \ \u0434\u043B\u044F \u0440\u043E\u0437\u0431\u043E\u0440\u0443 \u0442\u0430 \u0432\
  \u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F XML. \u0421\u043F\u043E\u0447\u0430\
  \u0442\u043A\u0443 \u0434\u0430\u0432\u0430\u0439\u0442\u0435 \u0440\u043E\u0437\
  \u0431\u0435\u0440\u0435\u043C\u043E \u0434\u0435\u044F\u043A\u0438\u0439 XML."
lastmod: '2024-03-13T22:44:48.694276-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u043F\u0440\u043E\u043F\u043E\u043D\u0443\u0454 \u0431\u0456\u0431\
  \u043B\u0456\u043E\u0442\u0435\u043A\u0443 `clojure.data.xml` \u0434\u043B\u044F\
  \ \u0440\u043E\u0437\u0431\u043E\u0440\u0443 \u0442\u0430 \u0432\u0438\u0432\u0435\
  \u0434\u0435\u043D\u043D\u044F XML."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

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
