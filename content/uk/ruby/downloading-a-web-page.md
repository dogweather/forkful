---
title:                "Ruby: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому

Користуватися Ruby для завантаження веб-сторінок - це швидкий і простий спосіб отримати доступ до інформації з Інтернету. Це особливо корисно для людей, які займаються веб-скрапінгом або потребують доступу до веб-сторінок для подальшої обробки даних.

## Як

```ruby
require 'open-uri'

# Використовуючи open-uri, ми можемо завантажувати веб-сторінки, передаючи їх URL у вигляді аргументу
response = open("https://www.example.com")
# Тепер ми можемо отримати доступ до HTML-коду сторінки, використовуючи метод .read
html = response.read
# Далі можемо обробити отриманий HTML-код за допомогою будь-якої Ruby-бібліотеки (наприклад, Nokogiri)
```

Виведення отриманого HTML-коду може виглядати приблизно так:

```html
<!DOCTYPE html>
<html>
  <head>
    <title>Example Website</title>
  </head>
  <body>
    <h1>Hello, world!</h1>
    <p>This is a sample webpage.</p>
  </body>
</html>
```

## Глибоке погруження

Метод `open` у модулі `open-uri` має багато корисних опцій, які можуть допомогти вам при завантаженні веб-сторінок. Наприклад, ви можете використовувати `read_timeout` для установки максимального часу очікування на відповідь веб-сервера. Також, ви можете використовувати метод `open` для завантаження файлів, передаючи другий аргумент з режимом "rb" (binary read).

## Дивитися також

- [Документація Ruby для модуля open-uri](https://ruby-doc.org/stdlib-2.6.3/libdoc/open-uri/rdoc/OpenURI.html)
- [Приклади використання open-uri в Ruby](https://www.rubyguides.com/2018/08/open-uri-ruby/)
- [W3Schools - введення в HTML](https://www.w3schools.com/html/html_intro.asp)