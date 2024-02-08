---
title:                "Завантаження веб-сторінки"
aliases:
- uk/ruby/downloading-a-web-page.md
date:                  2024-01-20T17:44:57.590980-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що і чому?

Завантаження веб-сторінки — це процес отримання даних з інтернету на ваш комп'ютер. Програмісти роблять це для обробки інформації, автоматизації завдань, або моніторингу контенту.

## Як саме:

Щоб завантажити веб-сторінку в Ruby, можна використати гем 'net/http'. Ось простий приклад:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/')
response = Net::HTTP.get(uri)

puts response
```

Якщо виконати цей код, Ruby виведе HTML код веб-сторінки example.com на екран.

## Поглиблений розбір:

Завантаження веб-сторінок почалося з перших днів інтернету. Історично, використовувались різні інструменти, такі як командна утиліта cURL або інтерактивні браузери. Однак, для спрощення цього процесу в програмуванні, були створені бібліотеки і геми. 

Для Ruby, перед 'net/http' часто використовували 'open-uri' для завантаження. Але зараз існують й інші вишукані бібліотеки, як-от 'Mechanize', 'HTTParty', або 'RestClient', які надають більше можливостей.

Завантаження веб-сторінки працює через HTTP протокол. Ruby відправляє HTTP GET запит на сервер, який господарює веб-сайт, і отримує відповідь у формі HTML, JSON, або іншого формату.

## Дивіться також:

- Документація по 'net/http': https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html
- Mechanize: https://github.com/sparklemotion/mechanize
- HTTParty: https://github.com/jnunemaker/httparty
- RestClient: https://github.com/rest-client/rest-client
- Ruby cURL: http://curl.haxx.se/libcurl/ruby/
