---
title:                "Надсилання http-запиту з основною автентифікацією"
html_title:           "Clojure: Надсилання http-запиту з основною автентифікацією"
simple_title:         "Надсилання http-запиту з основною автентифікацією"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

Є багато причин, чому хтось може хотіти відправити HTTP-запит з базовою автентифікацією. Наприклад, це може бути необхідно для доступу до захищених ресурсів чи для підтвердження ідентифікації користувача на веб-сайті.

## Як це зробити

Використовуючи бібліотеку `cljs-http`, дуже просто надіслати HTTP-запит з базовою автентифікацією. Варто зазначити, що для цього потрібно знати кодування веб-служби, яку необхідно автентифікувати. В цьому прикладі використовується кодування "UTF-8".

```Clojure
(ns basic-auth
  (:require [cljs-http.client :as http]))

; Надсилаємо GET-запит з базовою автентифікацією
; Замість "login" і "password" потрібно вказати свої дані
(http/get "https://example.com"
          {:basic-auth ["login" "password"]
           :content-type "application/json"})
```

Вивід буде наступним:

```Clojure
{:status 200,
 :headers {"Content-Length" "55",
           "Content-Type" "application/json",
           "Date" "Mon, 28 Sep 2020 12:00:00 GMT"},
 :body "{\"message\": \"Your request was successfully authenticated!\"}"}
```

## Глибокий дайв

Часто для взаємодії з веб-сайтами потрібно використовувати базову автентифікацію. Це простий і ефективний спосіб перевірити ідентифікацію користувача перед наданням доступу до захищеного ресурсу. Однак, варто пам'ятати, що базова автентифікація не надійна і може бути легко підроблена. Тому, для більш важливих ресурсів, краще використовувати інші методи автентифікації, наприклад, токени доступу.

## Приклади

- [Офіційна документація `cljs-http`](https://github.com/r0man/cljs-http)
- [Стаття про базову автентифікацію на MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)

## Дивіться також

- [Курс Clojure на Codeacademy](https://www.codecademy.com/learn/learn-clojure)