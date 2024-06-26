---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:29.476815-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Ruby \u0434\u0435\u043B\u0430\u0435\u0442 \u0441\u043A\u0430\u0447\
  \u0438\u0432\u0430\u043D\u0438\u0435 \u0432\u0435\u0431-\u0441\u0442\u0440\u0430\
  \u043D\u0438\u0446\u044B \u043F\u0440\u043E\u0441\u0442\u044B\u043C \u0441 \u043F\
  \u043E\u043C\u043E\u0449\u044C\u044E \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\
  \u043A \u0432\u0440\u043E\u0434\u0435 `net/http` \u0438 \u0433\u0435\u043C\u043E\
  \u0432, \u0442\u0430\u043A\u0438\u0445 \u043A\u0430\u043A `open-uri`. \u0412\u043E\
  \u0442 \u043A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\u0442\
  \u044C\u2026"
lastmod: '2024-03-13T22:44:45.995272-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u0434\u0435\u043B\u0430\u0435\u0442 \u0441\u043A\u0430\u0447\u0438\
  \u0432\u0430\u043D\u0438\u0435 \u0432\u0435\u0431-\u0441\u0442\u0440\u0430\u043D\
  \u0438\u0446\u044B \u043F\u0440\u043E\u0441\u0442\u044B\u043C \u0441 \u043F\u043E\
  \u043C\u043E\u0449\u044C\u044E \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\
  \ \u0432\u0440\u043E\u0434\u0435 `net/http` \u0438 \u0433\u0435\u043C\u043E\u0432\
  , \u0442\u0430\u043A\u0438\u0445 \u043A\u0430\u043A `open-uri`."
title: "\u0417\u0430\u0433\u0440\u0443\u0437\u043A\u0430 \u0432\u0435\u0431-\u0441\
  \u0442\u0440\u0430\u043D\u0438\u0446\u044B"
weight: 42
---

## Как это сделать:
Ruby делает скачивание веб-страницы простым с помощью библиотек вроде `net/http` и гемов, таких как `open-uri`. Вот как это сделать используя `net/http`:
```Ruby
require 'net/http'
require 'uri'

url = URI.parse('http://example.com')
response = Net::HTTP.get_response(url)

puts response.body if response.is_a?(Net::HTTPSuccess)
```

Вы получите HTML-содержимое `http://example.com` на печать.

Использование `open-uri` еще проще:

```Ruby
require 'open-uri'

downloaded_page = URI.open('http://example.com').read
puts downloaded_page
```

Опять же, содержимое веб-страницы отобразится в вашем терминале.

## Глубже
В старые времена интернета, скачивание страницы было немного более трудоемким и включало ручное создание HTTP-запросов. Сегодня Ruby значительно упрощает эту сложность.

Альтернативы `net/http` и `open-uri` включают более высокоуровневые гемы вроде `HTTParty` и `RestClient`. Они предлагают больше функций и объектно-ориентированный подход. Для серьезного веб-скрапинга многие рубисты обращаются к `Nokogiri` для разбора HTML или к `Mechanize`, который действует как веб-браузер.

С точки зрения реализации, имейте в виду, что `open-uri` является оберткой для `net/http`, поэтому он довольно удобен, но может не иметь некоторого низкоуровневого контроля. `net/http` дает вам больше контроля над запросом, но может быть многословным для простых задач.

## Смотрите также
Для дальнейшего чтения и дополнительных ресурсов обратите внимание на:

- Документация Ruby's Net::HTTP: [https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html)
- Документация Open-URI: [https://ruby-doc.org/stdlib-3.0.0/libdoc/open-uri/rdoc/OpenURI.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/open-uri/rdoc/OpenURI.html)
- Веб-страница Nokogiri: [https://nokogiri.org/](https://nokogiri.org/)
- Репозиторий гема Mechanize: [https://github.com/sparklemotion/mechanize](https://github.com/sparklemotion/mechanize)
- Гем HTTParty на GitHub: [https://github.com/jnunemaker/httparty](https://github.com/jnunemaker/httparty)
- Гем RestClient: [https://github.com/rest-client/rest-client](https://github.com/rest-client/rest-client)
