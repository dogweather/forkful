---
title:                "Надсилання http запиту з базовою аутентифікацією."
html_title:           "Ruby: Надсилання http запиту з базовою аутентифікацією."
simple_title:         "Надсилання http запиту з базовою аутентифікацією."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Що і Чому?
Надсилання HTTP-запиту з базовою аутентифікацією - це процес, коли програміст включає інформацію про аутентифікацію в HTTP-запит для доступу до певної веб-сторінки або апі. Це може бути корисно у випадках, коли веб-сайт вимагає логін та пароль для доступу або для безпечного передавання даних.

# Як це зробити:
```Ruby
require 'net/http'
require 'uri'

uri = URI('https://www.example.com')
req = Net::HTTP::Get.new(uri)
req.basic_auth('username', 'password')

res = Net::HTTP.start(uri.hostname, uri.port) do |http|
  http.request(req)
end

puts res.body
```
*Ви можете змінити метод HTTP-запиту та дані аутентифікації за своїм розсудом.*

# Глибше в шостий
Історичний контекст: базова аутентифікація була розроблена відразу після створення протоколу HTTP, тому це є однією з найстаріших і найбільш розповсюджених методів аутентифікації в веб-серверах.

Альтернативи: існують інші методи аутентифікації, такі як DIGEST і Bearer, які забезпечують більш безпечний спосіб доступу до веб-сайту чи апі.

Деталі реалізації: при використанні базової аутентифікації, назва користувача та пароль будуть закодовані у форматі Base64 та додані до заголовка запиту `Authorization`.

# Дивись також:
- [Документація Ruby по Net::HTTP](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
- [Стаття в блозі про різні методи аутентифікації в HTTP](https://www.redhat.com/en/blog/implementing-http-basic-authentication-stateless-and-stateful-applications)