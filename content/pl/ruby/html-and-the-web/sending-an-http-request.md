---
title:                "Wysyłanie żądania HTTP"
aliases: - /pl/ruby/sending-an-http-request.md
date:                  2024-01-20T18:00:39.376305-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie żądania HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/sending-an-http-request.md"
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
