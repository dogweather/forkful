---
title:                "Надсилання http-запиту."
html_title:           "Ruby: Надсилання http-запиту."
simple_title:         "Надсилання http-запиту."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому

Відправлення HTTP-запиту є важливою частиною веб-розробки, оскільки дозволяє взаємодіяти з іншими веб-серверами та отримувати потрібну інформацію з Інтернету.

## Як

```Ruby
require 'net/http'

url = URI("https://www.example.com/")
response = Net::HTTP.get_response(url)

puts response.code #=> "200"
puts response.body #=> "Hello, world!"
```

У першому рядку ми підключаємо необхідну бібліотеку `net/http`, а потім визначаємо URL-адресу, з якої хочемо отримати відповідь. Далі за допомогою методу `get_response` ми відправляємо GET-запит на вказаний URL та зберігаємо відповідь у змінну `response`. Тепер ми можемо отримати статус код відповіді та її тіло за допомогою виклику відповідних методів.

## Глибокий Занурення

HTTP-протокол є основою для взаємодії з веб-серверами та передачі даних в Інтернеті. Всі HTTP-запити складаються з трьох частин: методу, шляху та версії протоколу. Наприклад, у нашому прикладі, метод `GET` використовується для отримання ресурсу з вказаного шляху `/`, а версія протоколу - HTTP/1.1.

Якщо ви хочете надіслати HTTP-запит з параметрами, ви можете використовувати об'єкт `URI` для визначення шляху з параметрами, а потім відправити його за допомогою методу `get` або `post` замість `get_response`.

## Дивитися Також

- [Ruby Документація: net/http](https://ruby-doc.org/stdlib/libdoc/net/http/)
- [Приклади використання HTTP-запитів в Ruby](https://www.rubyguides.com/2018/08/ruby-http-request/)
- [Відправка і отримання HTTP-запитів з Ruby](https://www.digitalocean.com/community/tutorials/how-to-send-an-http-request-using-ruby)