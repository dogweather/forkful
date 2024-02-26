---
date: 2024-01-20 18:00:39.376305-07:00
description: "Wysy\u0142anie \u017C\u0105dania HTTP to spos\xF3b na komunikacj\u0119\
  \ Twojego programu z serwerem webowym; wy\u015Blij zapytanie i odbierz odpowied\u017A\
  . Programi\u015Bci to robi\u0105, aby\u2026"
lastmod: '2024-02-25T18:49:34.304969-07:00'
model: gpt-4-1106-preview
summary: "Wysy\u0142anie \u017C\u0105dania HTTP to spos\xF3b na komunikacj\u0119 Twojego\
  \ programu z serwerem webowym; wy\u015Blij zapytanie i odbierz odpowied\u017A. Programi\u015B\
  ci to robi\u0105, aby\u2026"
title: "Wysy\u0142anie \u017C\u0105dania HTTP"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Wysyłanie żądania HTTP to sposób na komunikację Twojego programu z serwerem webowym; wyślij zapytanie i odbierz odpowiedź. Programiści to robią, aby akcesować dane, interaktywować z API lub serwisami webowymi.

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
