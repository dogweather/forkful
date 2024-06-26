---
date: 2024-01-20 18:01:19.623118-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Clojure \u043C\u0430\u0454 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\
  \u0438, \u044F\u043A\u0456 \u0441\u043F\u0440\u043E\u0449\u0443\u044E\u0442\u044C\
  \ HTTP \u0437\u0430\u043F\u0438\u0442\u0438. \u041E\u0441\u044C \u044F\u043A \u0432\
  \u0456\u0434\u043F\u0440\u0430\u0432\u0438\u0442\u0438 \u0442\u0430\u043A\u0438\u0439\
  \ \u0437\u0430\u043F\u0438\u0442 \u0456\u0437 \u0431\u0430\u0437\u043E\u0432\u043E\
  \u044E \u0430\u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\
  \u0454\u044E, \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\
  \u044E\u0447\u0438 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0443\u2026"
lastmod: '2024-03-13T22:44:48.652954-06:00'
model: gpt-4-1106-preview
summary: "Clojure \u043C\u0430\u0454 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\
  \u043A\u0438, \u044F\u043A\u0456 \u0441\u043F\u0440\u043E\u0449\u0443\u044E\u0442\
  \u044C HTTP \u0437\u0430\u043F\u0438\u0442\u0438."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E"
weight: 45
---

## Як це зробити:
Clojure має бібліотеки, які спрощують HTTP запити. Ось як відправити такий запит із базовою аутентифікацією, використовуючи бібліотеку `clj-http`.

```Clojure
(require '[clj-http.client :as client])

(defn fetch-protected-resource [url username password]
  (let [auth-str (str username ":" password)
        encoded-auth-str (-> auth-str (.getBytes) java.util.Base64/getEncoder (.encodeToString))]
    (client/get url {:headers {"Authorization" (str "Basic " encoded-auth-str)}})))

;; Приклад використання:
(println (fetch-protected-resource "http://example.com/protected" "myUsername" "myPassword"))
```

Ця функція кодує логін і пароль в Base64 і вставляє їх у заголовок `Authorization`.

## Глибше Занурення:
Базова аутентифікація (`Basic Auth`) — старий, простий метод аутентифікації через HTTP. Ім'я користувача та пароль поєднують, кодують за допомогою Base64, і передають у заголовку `Authorization`. Його легко використати, але він не є найбезпечнішим методом, оскільки дані можна розкодувати. HTTPS мінімізує ризик.

Альтернативи базовій аутентифікації включають OAuth, API ключі, токени тощо. Ці методи забезпечують додаткові рівні безпеки і часто використовуються для сучасних API.

У Clojure та інших мовах програмування є багато бібліотек для відправки HTTP запитів. `clj-http` проста в користуванні і добре інтегрується з Clojure проектами. Ви напряму працюєте з заголовками, вмістом запитів, і параметрами — це дає можливість гнучко налаштовувати свої HTTP запити.

## Дивись також:
- [clj-http GitHub Repository](https://github.com/dakrone/clj-http) - бібліотека clj-http.
- [HTTP Basic Auth](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme) - MDN веб-документація по базовій аутентифікації.
- [ClojureDocs](https://clojuredocs.org/) - офіційна документація Clojure з прикладами від спільноти.
