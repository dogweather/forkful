---
title:                "Розбір HTML"
html_title:           "Arduino: Розбір HTML"
simple_title:         "Розбір HTML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Що і чому?
Парсинг HTML - це процес витягування специфічної інформації з HTML-документів. Програмісти роблять це, щоб автоматизувати обробку даних з веб-сторінок.

## Як це зробити:
Clojure надає бібліотеку, яка дозволяє нам парсити HTML документи. Нагадую, що ми працюємо з синтаксисом XML, отже, маємо знати його основи.

```Clojure
(require '[net.cgrand.enlive-html :as html])

(defn fetch-html [url]
 (html/html-resource (java.net.URL. url)))

(def parsed-html 
  (fetch-html "http://somesite.org"))
```

Вищенаведений код: витягуємо HTML соместрічку і парсимо її. `fetch-html` - це функція, яка витягує HTML з URL.

## Поглиблений розбір
В історичному контексті, парсинг HTML був складним процесом і потребував багато коду. Але з розвитком мов, як Clojure, це стало набагато простіше.

Альтернативами для парсингу HTML в Clojure є бібліотеки, такі як JSoup або HtmlUnit, хоча вони можуть бути більш складними в використанні.

Використовуючи `enlive-html`, наш HTML документ розбивається на дерево вузлів, яке ми можемо легко переглядати та маніпулювати.

## Дивіться також
3. [Офіційна документація Enlive](https://github.com/cgrand/enlive). 
4. [Про XML в Clojure](https://clojuredocs.org/clojure.xml)