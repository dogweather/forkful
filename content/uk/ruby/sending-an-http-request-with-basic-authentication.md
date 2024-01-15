---
title:                "Надсилання запиту http з основною автентифікацією"
html_title:           "Ruby: Надсилання запиту http з основною автентифікацією"
simple_title:         "Надсилання запиту http з основною автентифікацією"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому
Надіслати запит HTTP з основною автентифікацією є необхідністю при роботі з більшістю віддалених API інтерфейсів. Це забезпечує безпеку та доступ до обміну даними з обраним сервером.

## Як
```ruby
require 'net/http'

uri = URI('http://example.com/api/resource/123') # Змініть URL на свій відповідно до API.
req = Net::HTTP::Get.new(uri)
req.basic_auth 'username', 'password' # Замініть ім'я користувача та пароль на свої.

res = Net::HTTP.start(uri.hostname, uri.port) {|http|
  http.request(req)
}

puts res.body # Виведе тіло відповіді з сервера.
```

## Глибинний занурення
Базова автентифікація (basic authentication) використовується для здійснення доступу до веб-ресурсів і забезпечує простий рівень контролю користувачів. Для виконання запиту з базовою автентифікацією потрібно включити заголовок "Authorization" з інформацією про ім'я користувача та пароль в форматі "username:password", зашифрованим у форматі Base64. Якщо сервер не може їх розшифрувати, запит буде відхилений з помилкою "401 Unauthorized".

## Дивись також
- [Офіційна документація Ruby з прикладами коду](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
- [Стаття про базову автентифікацію на MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- [Більш детальна стаття про використання базової автентифікації в Ruby](https://www.rubyguides.com/2018/08/ruby-net-http/)