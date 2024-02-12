---
title:                "Отправка HTTP-запроса"
aliases:
- /ru/ruby/sending-an-http-request/
date:                  2024-01-29T00:12:24.816306-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/ruby/sending-an-http-request.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Отправка HTTP-запроса означает запрос данных с ресурса в Интернете. Программисты делают это для взаимодействия с API, сбора данных с веб-страниц или общения с серверами.

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
