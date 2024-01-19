---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що і навіщо?

Завантаження веб-сторінки - це процес отримання вмісту сторінки через Інтернет. Програмісти цим займаються, коли їм потрібно обробити дані з сайту автоматично, наприклад, для аналізу даних.

## Як це робити:

В Clojure для завантаження веб-сторінки ми можемо використовувати бібліотеку `clj-http`. Ось як це можна зробити:

```clojure
(require '[clj-http.client :as client])

(let [response (client/get "https://example.com")]
  (println (:status response))
  (println (:body response)))
```

Це виведе код статусу відповіді та вміст веб-сторінки.

## Глибше занурення:

#### Історичний контекст:
Clojure, як і багато високорівневих мов, пропонує декілька чудових бібліотек для роботи з HTTP. `clj-http` - одна з найпопулярніших, благодарі своєму багатому API і гнучкості.

#### Альтернативи:
Існують також інші бібліотеки, такі як `http-kit` та `aleph`, які також можуть бути використані для завантаження веб-сторінок.

#### Деталі реалізації:
Коли ви використовуєте `clj-http` для відправки HTTP-запиту, він використовує Java-бібліотеку Apache HttpComponents під капотом. Він дозволяє вам надсилати різні типи HTTP-запитів і добре працює з великими обсягами даних.

## Дивіться також:

- [clj-http GitHub](https://github.com/dakrone/clj-http)
- [http-kit GitHub](https://github.com/http-kit/http-kit)
- [aleph GitHub](https://github.com/ztellman/aleph)
- [Apache HttpComponents](https://hc.apache.org/)