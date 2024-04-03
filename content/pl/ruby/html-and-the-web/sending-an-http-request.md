---
date: 2024-01-20 18:00:39.376305-07:00
description: "Jak to zrobi\u0107: Ruby u\u017Cywa kilku gem\xF3w do obs\u0142ugi HTTP,\
  \ jak `net/http` \u2013 wbudowane i gotowe do u\u017Cycia. Oto przyk\u0142ad."
lastmod: '2024-03-13T22:44:35.929669-06:00'
model: gpt-4-1106-preview
summary: "Ruby u\u017Cywa kilku gem\xF3w do obs\u0142ugi HTTP, jak `net/http` \u2013\
  \ wbudowane i gotowe do u\u017Cycia."
title: "Wysy\u0142anie \u017C\u0105dania HTTP"
weight: 44
---

## Jak to zrobić:
Ruby używa kilku gemów do obsługi HTTP, jak `net/http` – wbudowane i gotowe do użycia. Oto przykład:

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/index.html')
response = Net::HTTP.get(uri)

puts response
```

Jeśli uruchomisz ten kod, zobaczysz zawartość strony `http://example.com/index.html` wyświetlającą się w konsoli.

## Deep Dive
Zanim powstały gemy takie jak `net/http`, `httparty` czy `rest-client`, komunikacja HTTP była bardziej skomplikowana. Musiałeś samodzielnie radzić sobie z niższym poziomem szczegółów, takich jak tworzenie gniazd sieciowych i ręczne parsowanie odpowiedzi HTTP. Na szczęście, te biblioteki ułatwiają zadanie, abstrahując większość złożoności.

Alternatywami dla `net/http` są `httparty` i `rest-client`, które oferują bardziej intuicyjne API:

```Ruby
# Z 'httparty'
require 'httparty'

response = HTTParty.get('http://example.com/index.html')
puts response.body
```

```Ruby
# Z 'rest-client'
require 'rest-client'

response = RestClient.get('http://example.com/index.html')
puts response.body
```

Implementacje mogą różnić się w zależności od potrzeb – `net/http` jest dobry do prostych rzeczy, ale dla większych aplikacji warto rozważyć coś bardziej wydajnego. Pamiętaj, że przy każdym z tych rozwiązań musisz obsłużyć możliwe wyjątki i błędy połączenia.

## Zobacz także:
- Ruby Dokumentacja `Net::HTTP`: https://ruby-doc.org/stdlib/libdoc/net/http/rdoc/Net/HTTP.html
- `HTTParty` gem: https://github.com/jnunemaker/httparty
- `Rest-Client` gem: https://github.com/rest-client/rest-client
- Przewodnik po HTTP w Ruby: https://www.rubyguides.com/2018/08/ruby-http-request/
