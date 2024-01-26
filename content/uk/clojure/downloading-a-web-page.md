---
title:                "Завантаження веб-сторінки"
date:                  2024-01-20T17:43:37.806340-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
Завантаження веб-сторінки — це процес отримання її вмісту через Інтернет. Програмісти роблять це, щоб аналізувати дані, моніторити зміни або витягувати інформацію для подальшої обробки.

## Як це зробити:
```clojure
(require '[org.httpkit.client :as http])

(defn download-page [url]
  (let [response (http/get url)]
    (-> response
        @(:body))))

(println (download-page "http://example.com"))
```
Вищезазначений код асинхронно завантажує веб-сторінку та виводить її вміст у консоль.

## Глибоке занурення
Історично, для завантаження веб-сторінок у Clojure використовувалися бібліотеки як `clj-http` чи `http-kit`. Альтернативно, можна застосовувати Java бібліотеки через Java інтероперабельність. Важливим є розуміння того, що більшість HTTP-клієнтів у Clojure працюють асинхронно, тому зазвичай вам потрібно розібратись з промісами або майбутніми результатами (futures).

## Див.також
- [http-kit GitHub repository](https://github.com/http-kit/http-kit)
- [clj-http: A Clojure HTTP library](https://github.com/dakrone/clj-http)
- [ClojureDocs - Guide to Clojure Core.async](https://clojuredocs.org/clojure.core.async)
