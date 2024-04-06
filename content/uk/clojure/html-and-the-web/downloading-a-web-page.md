---
date: 2024-01-20 17:43:37.806340-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0412\u0438\u0449\u0435\u0437\u0430\u0437\u043D\u0430\u0447\u0435\u043D\u0438\
  \u0439 \u043A\u043E\u0434 \u0430\u0441\u0438\u043D\u0445\u0440\u043E\u043D\u043D\
  \u043E \u0437\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0443\u0454 \u0432\u0435\
  \u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0443 \u0442\u0430 \u0432\u0438\
  \u0432\u043E\u0434\u0438\u0442\u044C \u0457\u0457 \u0432\u043C\u0456\u0441\u0442\
  \ \u0443 \u043A\u043E\u043D\u0441\u043E\u043B\u044C."
lastmod: '2024-04-05T21:53:48.904217-06:00'
model: gpt-4-1106-preview
summary: "\u0412\u0438\u0449\u0435\u0437\u0430\u0437\u043D\u0430\u0447\u0435\u043D\
  \u0438\u0439 \u043A\u043E\u0434 \u0430\u0441\u0438\u043D\u0445\u0440\u043E\u043D\
  \u043D\u043E \u0437\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0443\u0454 \u0432\
  \u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0443 \u0442\u0430 \u0432\
  \u0438\u0432\u043E\u0434\u0438\u0442\u044C \u0457\u0457 \u0432\u043C\u0456\u0441\
  \u0442 \u0443 \u043A\u043E\u043D\u0441\u043E\u043B\u044C."
title: "\u0417\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F \u0432\
  \u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0438"
weight: 42
---

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
