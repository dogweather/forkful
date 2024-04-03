---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:12:24.816306-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Ruby \u0434\u0435\u043B\u0430\u0435\u0442 \u043E\u0442\u043F\u0440\
  \u0430\u0432\u043A\u0443 HTTP-\u0437\u0430\u043F\u0440\u043E\u0441\u043E\u0432 \u0434\
  \u043E\u0432\u043E\u043B\u044C\u043D\u043E \u043F\u0440\u043E\u0441\u0442\u043E\u0439\
  . \u0412\u043E\u0442 \u0441\u0430\u043C\u044B\u0439 \u0431\u044B\u0441\u0442\u0440\
  \u044B\u0439 \u0441\u043F\u043E\u0441\u043E\u0431 \u0441 \u0438\u0441\u043F\u043E\
  \u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\u043C \u0441\u0442\u0430\u043D\
  \u0434\u0430\u0440\u0442\u043D\u043E\u0439 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\
  \u0435\u043A\u0438 Net::HTTP."
lastmod: '2024-03-13T22:44:45.991901-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u0434\u0435\u043B\u0430\u0435\u0442 \u043E\u0442\u043F\u0440\u0430\
  \u0432\u043A\u0443 HTTP-\u0437\u0430\u043F\u0440\u043E\u0441\u043E\u0432 \u0434\u043E\
  \u0432\u043E\u043B\u044C\u043D\u043E \u043F\u0440\u043E\u0441\u0442\u043E\u0439."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430"
weight: 44
---

## Как это сделать:
Ruby делает отправку HTTP-запросов довольно простой. Вот самый быстрый способ с использованием стандартной библиотеки Net::HTTP.

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com')
response = Net::HTTP.get(uri)
puts response
```

Это выведет HTML содержимое `http://example.com`.

Возможно, вы захотите также отправлять данные:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/api')
res = Net::HTTP.post_form(uri, 'key1' => 'value1', 'key2' => 'value2')
puts res.body
```

Это отправляет POST запрос с данными и показывает ответ.

## Подробнее:
Раньше отправка HTTP-запросов была более сложной, и возможно, вам нужно было использовать gem, такой как `HTTParty`. Но встроенная библиотека Ruby `Net::HTTP` проделала большой путь. Теперь она поддерживает большинство необходимых вещей.

Однако, `Net::HTTP` может быть многословной. Если ваш проект требует больше функциональности HTTP или синтаксического сахара, `HTTParty` или `Faraday` являются отличными альтернативами. Эти гемы предоставляют более выразительный API и могут обрабатывать более сложные сценарии, такие как промежуточное ПО или разные адаптеры.

В основе отправки HTTP-запроса с Ruby лежит создание HTTP-клиента, настройка объекта запроса с методом, заголовками и телом при необходимости, а затем отправка запроса для получения ответа.

Пример с HTTParty:

```Ruby
require 'httparty'

response = HTTParty.get('http://example.com')
puts response.body
```

Это делает то же самое, что и `Net::HTTP.get`, но с меньшей конфигурацией.

## Смотрите также:
Для более детальной информации очень полезны документы Ruby:
- Net::HTTP: https://ruby-doc.org/stdlib/libdoc/net/http/rdoc/Net/HTTP.html
- HTTParty: https://github.com/jnunemaker/httparty
- Faraday: https://lostisland.github.io/faraday/

И если у вас большой аппетит к сетевым возможностям HTTP в Ruby, загляните сюда:
- Ruby's Open URI: https://ruby-doc.org/stdlib/libdoc/open-uri/rdoc/OpenURI.html
- WebMock для тестирования HTTP-запросов: https://github.com/bblimke/webmock
