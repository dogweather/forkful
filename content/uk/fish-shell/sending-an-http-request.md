---
title:                "Fish Shell: Надсилання http запиту."
simple_title:         "Надсилання http запиту."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому

Надсилання запиту HTTP є важливою частиною програмування. Це дозволяє зробити з'єднання з веб-сайтами та отримати необхідну інформацію з сервера. Наприклад, ви можете отримати дані з API, яке надає різні сервіси в мережі.

## Як

Існує багато способів надсилання HTTP запитів, але у цьому блозі ми покажемо, як зробити це за допомогою Fish Shell. Нижче подано код для створення базового запиту GET на сайт Google та виводу отриманої відповіді:

```Fish Shell
curl https://www.google.com
```

Буде виведено HTML-код головної сторінки Google. Також можна додати параметри запиту, як показано у прикладі нижче:

```Fish Shell
curl https://www.google.com/?q=fish+shell
```

Тепер запит буде шукати слово "fish shell" у Google та повертати результати пошуку.

## Глибше

Fish Shell надає багато можливостей для надсилання HTTP запитів та маніпулювання отриманими даними. Тут ви можете дізнатися більше про параметри запитів, авторизацію та обробку JSON даних. Також, для більшої гнучкості, ви можете використовувати змінну "http_proxy" для налаштування проксі-сервера для відправки запитів.

## Дивіться також

- [Офіційна документація Fish Shell](http://fishshell.com/docs/)
- [Авторизація за допомогою Fish Shell](https://github.com/deanishe/alfred-http-auth/wiki/Curl-basics) 
- [Обробка JSON даних з Fish Shell](https://fishshell.com/docs/current/tutorial.html#json)