---
title:                "Надсилання http-запиту"
html_title:           "Bash: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому

Відправлення HTTP запитів є необхідною складовою нашого розробленого світу. Попри те, що вона може здатися складною, ми швидко з'ясуємо, що це насправді простий та потужний інструмент для взаємодії з веб-службами та додатками.

## Як

```Bash
# Простий GET запит до сайту, вивід потоку даних
curl https://example.com
```

```Bash
# Відправлення POST запиту з JSON данними та вивід коду відповіді
curl -X POST -d '{"name":"John", "email":"john@example.com"}' https://example.com
```

```Bash
# Запит з іншою методою та налаштуваннями хедерів
curl -X PUT -H "Content-Type: application/json" -d '{"username":"Logan", "password":"123456"}' https://example.com/api/login
```

```Bash
# Запит з авторизаційним токеном та вивід результату в файл
curl -H "Authorization: Bearer abcdefg12345" https://example.com/api/alerts > alerts.json
```

## Поглиблення

HTTP (Hypertext Transfer Protocol) є основним протоколом для обміну даними між клієнтом та сервером в Інтернеті. Запити передаються з клієнта до сервера та пов'язані з URL-адресою, методом та даними. Існує багато різних методів, які можна використовувати в HTTP запитах, таких як GET, POST, PUT, DELETE і PATCH. Кожен метод виконує різні дії, що дає нам більше гнучкості у взаємодії з веб-службами та додатками. Крім того, ми також можемо встановлювати хедери у наших запитах, що дозволяє нам передавати додаткові метадані. Ми можемо використовувати різні інструменти, такі як curl та wget, для відправлення HTTP запитів у командному рядку.

## Дивись також

- [Документація по HTTP запитах в Bash](https://curl.se/docs/httpscripting.html)
- [Повний посібник по використанню curl](https://linuxize.com/post/curl-command-examples/)