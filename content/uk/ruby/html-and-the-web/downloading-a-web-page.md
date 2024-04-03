---
date: 2024-01-20 17:44:57.590980-07:00
description: "\u042F\u043A \u0441\u0430\u043C\u0435: \u0429\u043E\u0431 \u0437\u0430\
  \u0432\u0430\u043D\u0442\u0430\u0436\u0438\u0442\u0438 \u0432\u0435\u0431-\u0441\
  \u0442\u043E\u0440\u0456\u043D\u043A\u0443 \u0432 Ruby, \u043C\u043E\u0436\u043D\
  \u0430 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u0442\u0438 \u0433\
  \u0435\u043C 'net/http'. \u041E\u0441\u044C \u043F\u0440\u043E\u0441\u0442\u0438\
  \u0439 \u043F\u0440\u0438\u043A\u043B\u0430\u0434."
lastmod: '2024-03-13T22:44:50.225781-06:00'
model: gpt-4-1106-preview
summary: "\u0429\u043E\u0431 \u0437\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0438\
  \u0442\u0438 \u0432\u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0443\
  \ \u0432 Ruby, \u043C\u043E\u0436\u043D\u0430 \u0432\u0438\u043A\u043E\u0440\u0438\
  \u0441\u0442\u0430\u0442\u0438 \u0433\u0435\u043C 'net/http'."
title: "\u0417\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F \u0432\
  \u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0438"
weight: 42
---

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
