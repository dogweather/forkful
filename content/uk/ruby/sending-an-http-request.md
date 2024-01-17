---
title:                "Надсилання http-запиту"
html_title:           "Ruby: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Що та чому?
Відправка HTTP запиту є важливою технікою у веб програмуванні. Це є процес, при якому програма надсилає запит серверу, щоб отримати потрібну інформацію. Програмісти роблять це, щоб забезпечити взаємодію між веб-додатками та розв'язати багато проблем з даними.

# Як це зробити:
Найпростіший шлях взаємодії з веб-додатком - відправка HTTP запиту. Для цього потрібно використовувати вбудовану бібліотеку Net::HTTP у Ruby. Ось приклад коду, який демонструє відправку GET запиту до веб-сайту і виводить отриману відповідь:

```Ruby
require 'net/http'

url = URI('https://example.com')
response = Net::HTTP.get_response(url)

puts response.body
```

В результаті на консоль буде виведено вміст сторінки https://example.com.

# Глибокий занурення:
Відправка HTTP запиту була розроблена в 1990-х роках для забезпечення зв'язку між серверами та клієнтами у веб-серверних додатках. На сьогоднішній день є багато альтернативних бібліотек для взаємодії з веб-додатками, таких як Faraday, HTTParty та Rest-client. Однак, бібліотека Net::HTTP залишається важливою для більшості простих задач взаємодії з веб-серверами.

Коли ви надсилаєте HTTP запит, ви можете вказати різні методи запиту, такі як GET, POST, PUT або DELETE. Крім цього, можна передавати різні параметри та заголовки у відправленому запиті.

# Дивіться також:
- [Ruby документація Net::HTTP](https://ruby-doc.org/stdlib-2.7.2/libdoc/net/http/rdoc/Net/HTTP.html)
- [Faraday](https://github.com/lostisland/faraday)
- [HTTParty](https://github.com/jnunemaker/httparty)
- [Rest-client](https://github.com/rest-client/rest-client)