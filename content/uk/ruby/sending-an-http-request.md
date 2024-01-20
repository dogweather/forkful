---
title:                "Відправлення HTTP-запиту"
html_title:           "Bash: Відправлення HTTP-запиту"
simple_title:         "Відправлення HTTP-запиту"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що і чому?

Відправка HTTP-запиту - це засіб комунікації з веб-сервером або API. Програмісти роблять це для виконання операцій, як от отримання, оновлення або видалення даних.

## Як це робити:

Для відправлення HTTP-запиту в Ruby можна використовувати бібліотеку Net::HTTP. Давайте спробуємо відправити GET-запит.

```Ruby
require 'net/http'
require 'uri'

uri = URI.parse("http://example.com/")
response = Net::HTTP.get_response(uri)

puts response.body
```

Ця програма з’єднується з веб-сервером на example.com та друкує тіло відповіді.

## Поглиблено:

1. Історичний контекст: HTTP-запити на початку були задумані як простий спосіб обміну гіпертекстовою інформацією через інтернет.
2. Альтернативи: До альтернатив бібліотеки Net::HTTP в Ruby належать Faraday та HTTParty. Вони можуть бути простішими у використанні в деяких випадках.
3. Деталі реалізації: При відправленні HTTP-запиту програма створює TCP-з'єднання з веб-сервером, відправляє запит, отримує відповідь і закриває з'єднання.

## Див. також:

* Офіційна документація Ruby для Net::HTTP: https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html
* Faraday: https://lostisland.github.io/faraday/
* HTTParty: https://github.com/jnunemaker/httparty