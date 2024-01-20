---
title:                "Надсилання http-запиту"
html_title:           "Arduino: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що таке та навіщо потрібно?

Надсилання HTTP-запиту - це процес, при якому ваш код "запитує" інформацію від сервера. Ми робимо це для отримання, відправки, оновлення або видалення даних на віддаленому сервері.

## Як це працює:

```Javascript
fetch('https://api.github.com/users/github')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.error('Error:', error));
```
Ця функція відправляє запит до посилання URL, потім конвертує відповідь в формат JSON, а потім виводить результат в консоль. Якщо сталася помилка, вона буде виведена в консоль.

## Поглиблений аналіз:

HTTP-запити були вперше представлені в 1990-их для комунікації між браузерами і веб-серверами. Існують альтернативи HTTP-запитам, такі як WebSockets, які використовуються для неперервного обміну даними.

Щодо деталей втілення, метод fetch використовує протокол HTTP або HTTPS в залежності від URL-адреси. Результатом є об'єкт Promise, який представляє відгук на запит.

## Дивись також:

Не забудьте ознайомитися з іншими ресурсами, які допоможуть вам вивчити цю тему глибше:

1. [Mozilla Developer Network (MDN) - Fetch API](https://developer.mozilla.org/uk/docs/Web/API/Fetch_API/Using_Fetch)
2. [MDN - HTTP запити](https://developer.mozilla.org/uk/docs/Web/HTTP/Methods)
3. [Уроки JavaScript на Codecademy](https://www.codecademy.com/learn/introduction-to-javascript)