---
title:                "Загрузка веб-страницы"
date:                  2024-01-28T23:57:29.476815-07:00
model:                 gpt-4-0125-preview
simple_title:         "Загрузка веб-страницы"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/ruby/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Скачивание веб-страницы означает получение HTML-содержимого из интернета. Программисты делают это для разбора данных, извлечения информации или программного отслеживания изменений.

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
