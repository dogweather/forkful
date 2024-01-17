---
title:                "Надсилання http-запиту"
html_title:           "Clojure: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

Що & Чому?: Обмін даними за допомогою мережі Інтернет є постійною частиною програмування. Відправка HTTP запиту є одним з способів отримати дані з сервера і використовувати їх у вашій програмі.

Як & Що: Надсилання HTTP запиту у Clojure є просто за допомогою функції ```http-client``` з бібліотеки ```clj-http```. Ця функція дозволяє вам вказати метод (GET, POST, PUT, DELETE), URL і параметри запиту, а також обробити отриману відповідь сервера. Нижче надані приклади коду та виводу для використання GET і POST запитів:

```Clojure
;; Використовуємо GET запит для отримання заголовку головної сторінки Google:
(http-client {:method :get :url "http://www.google.com"})

// Вивід: 
;; {:status 200, :headers {"content-type" "text/html; charset=ISO-8859-1"}, :body "<html>..."}
 
;; Використовуємо POST запит для створення нового об'єкта на сервері:
(http-client {:method :post :url "http://www.example.com/objects" :params {:name "object1", :value "10"}})

// Вивід: 
;; {:status 201, :headers {"content-type" "application/json"}, :body "{\"id\":1,\"name\":\"object1\",\"value\":10}"}
```

Глибоке занурення: HTTP протокол був розроблений в 1991 році і є основою обміну даними в Інтернеті. У Clojure також є інші бібліотеки для взаємодії з сервером, такі як ```clj-http-lite``` і ```http-kit```, проте ```clj-http``` є однією з найпопулярніших і використовується в багатьох проектах. Крім того, Clojure також надає зручні інструменти для роботи з отриманими даними, такі як бібліотека ```clojure.data.json```, що дозволяє розбирати та створювати JSON об'єкти.

Дивіться також: Більше інформації про використання HTTP запитів у Clojure можна знайти на офіційних документаціях бібліотеки ```clj-http```. Для додаткової інформації про засоби обробки даних у Clojure, дивіться документацію бібліотеки ```clojure.data.json```.