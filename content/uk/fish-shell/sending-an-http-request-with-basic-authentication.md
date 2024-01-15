---
title:                "Надсилання http-запиту з базовою автентифікацією"
html_title:           "Fish Shell: Надсилання http-запиту з базовою автентифікацією"
simple_title:         "Надсилання http-запиту з базовою автентифікацією"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

Надіслати HTTP-запит з основною аутентифікацією на пошук ресурсів чи отримання доступу до захищених даних.

## Як

```Fish Shell``` надає зручний спосіб використання основної аутентифікації при відправці HTTP-запитів. Спочатку необхідно створити змінні для імені користувача (```username```) та пароля (```password```).

```Fish Shell
set -x username "user"
set -x password "password"
```

Потім можна використовувати змінні у параметрі ```-u```, коли виконуєте команду ```curl```, для передачі даних основної аутентифікації.

```Fish Shell
curl -u $username:$password https://example.com
```

Результатом буде отримання вмісту з ресурсу ```https://example.com```.

## Глибоке занурення

Такий підхід до основної аутентифікації дозволяє використовувати її для різних типів HTTP-запитів, таких як GET, POST, PUT, DELETE тощо. Також можна вказати змінні ```$username``` та ```$password``` як параметри у скрипті, що дозволить підтримувати різні комбінації логіна та пароля для різних запитів.

## Дивись також

- [Офіційна документація Fish Shell](https://fishshell.com/docs/current/)
- [Різні способи передачі даних у HTTP-запитах](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)