---
title:                "Надсилання HTTP-запиту"
date:                  2024-01-20T17:59:50.502936-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що і чому?
HTTP-запит - це спосіб, яким ваша програма просить інформацію з сервера. Програмісти використовують HTTP-запити для взаємодії із веб-сервісами, отримання даних, відправки форм, авторизації користувачів і багато іншого.

## Як це робити:
Для відправлення HTTP-запитів у Bash можна використовувати `curl` або `wget`. Ось базові приклади:

```Bash
# Отримання вмісту сторінки за допомогою curl
curl http://example.com

# Відправлення POST-запиту з даними форми
curl -d "param1=value1&param2=value2" -X POST http://example.com/resource

# Отримання веб-сторінки за допомогою wget
wget http://example.com
```
Вивід цих команд залежить від веб-сервера, з яким ви взаємодієте. Ваша командна оболонка покаже HTML, JSON або інший відповідь від сервера.

## Поглиблений аналіз
HTTP-запити існують з моменту створення протоколу HTTP у 1991 році. `curl` і `wget` — два різних інструменти для відправлення запитів, але вони не єдині. Є й інші, наприклад, `HTTPie`.

`curl` більш гнучкий і функціональний, в той час як `wget` спрощений та гарно підходить для завантаження файлів. `curl` підтримує багато протоколів (HTTP, HTTPS, FTP та інші), відправлення даних форм і навіть авторизації OAuth. 

З іншого боку, `wget` працює рекурсивно і здатен завантажити цілу веб-сторінку або сайт. Коли вибираєте між ними, думайте про ваші цілі: `curl` для більш контрольованих запитів, `wget` для завантаження вмісту.

## Дивіться ще:
- [cURL Manual](https://curl.se/docs/manual.html) - документація по `curl`.
- [Wget Manual](https://www.gnu.org/software/wget/manual/wget.html) - документація по `wget`.
- [HTTPie](https://httpie.io/) - сучасний, зручний інструмент для HTTP-запитів.
- [Bash Scripting Tutorial](https://ryanstutorials.net/bash-scripting-tutorial/) - тьюторіал з Bash скриптінгу.