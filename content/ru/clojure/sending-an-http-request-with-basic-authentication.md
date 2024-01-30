---
title:                "Отправка HTTP-запроса с базовой аутентификацией"
date:                  2024-01-29T00:02:52.315750-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса с базовой аутентификацией"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/clojure/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Отправка HTTP-запроса с базовой аутентификацией включает добавление имени пользователя и пароля к запросу за ограниченными ресурсами. Программисты делают это для доступа к API или веб-службам, которые требуют определенного уровня безопасности.

## Как это сделать:

В Clojure вы обычно используете библиотеку `clj-http` для HTTP-запросов, включая те, что с базовой аутентификацией. Давайте начнем с добавления зависимости (`[clj-http "3.12.3"]` на момент последнего обновления) в ваш `project.clj`.

Далее, вот как вы создаете GET-запрос с базовой аутентификацией:

```clojure
(require '[clj-http.client :as client])

(let [response (client/get "https://your-api.com/resource"
                           {:basic-auth ["username" "password"]})]
  (println "Статус:" (:status response))
  (println "Тело:" (:body response)))
```
Замените `"https://your-api.com/resource"`, `"username"` и `"password"` на ваши данные. Этот код отправляет GET-запрос и выводит статус и тело ответа.

Пример вывода может выглядеть примерно так:

```
Статус: 200
Тело: {JSON данные или что-то еще здесь}
```

## Подробный Разбор

Базовая HTTP аутентификация имеет корни в ранних веб-протоколах. Она передает имя пользователя и пароль в заголовке HTTP, закодированные с использованием Base64. Несмотря на свою простоту, она не самая безопасная, поскольку учетные данные могут быть легко расшифрованы, если они перехватываются.

Альтернативы:
- **Digest аутентификация**: Более сложная, включает отправку хэшированной версии пароля.
- **OAuth**: Более надежная система авторизации, которая не требует отправки имени пользователя и пароля.
- **API ключи**: Уникальные токены, используемые вместо традиционных учетных данных для входа.

В библиотеке `clj-http`, указание `:basic-auth` в хэшмапе опций заставляет библиотеку кодировать ваши учетные данные и прикреплять их к заголовку HTTP `Authorization`. Когда сервер получает запрос, он декодирует заголовок и проверяет учетные данные.

Имейте в виду, что для безопасной передачи следует использовать HTTPS, чтобы предотвратить перехват ваших учетных данных другими лицами.

## Смотрите Также

- GitHub репозиторий clj-http: https://github.com/dakrone/clj-http
- Официальная документация Clojure: https://clojure.org/
- Аутентификация HTTP на MDN: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication 
- Понимание OAuth: https://oauth.net/