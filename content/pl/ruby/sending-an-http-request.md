---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Wysyłanie żądania HTTP to prośba wysłana do serwera, aby udostępnił potrzebne nam dane. Programiści robią to, aby uzyskać dostęp do danych, z którymi chcą pracować, bez konieczności ich przechowywania lokalnie.

## Jak to zrobić:
Najprostszym sposobem na wysłanie żądania HTTP w Ruby jest użycie gemu 'net/http'. Oto przykładowe użycie:

```Ruby
require 'net/http'

url = URI("http://example.com/")
response = Net::HTTP.get(url)

puts response
```

Wykonanie powyższego kodu zwróci ciało żądania jako napis.

```Ruby
# Output
""Hello world"
```

## Pogłębiona Analiza
Ruby nie zawsze miało takie możliwości. W rzeczywistości, obsługa HTTP została dodana już w okolicach Ruby 1.8. WCześniej, musieliśmy polegać na narzędziach zewnętrznych, jak curl.

Jest wiele alternatyw dla 'net/http', takich jak 'open-uri' lub 'HTTParty', które oferują bardziej rozbudowane funkcje i łatwiejszy interfejs. W przypadku bardziej skomplikowanych zapytań, mogą okazać się lepszym wyborem.

Wysyłanie żądania HTTP to dość proste zadanie - od strony implementacji najtrudniejszym elementem jest prawdopodobnie obsługa błędów i sytuacji nietypowych. ULatwia to zrozumienie, jak działa protokół HTTP i jakie kody statusu można oczekiwać.

## Zobacz również
- [Dokumentacja net/http](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
- [Dokładny opis protokołu HTTP](https://developer.mozilla.org/pl/docs/Web/HTTP/Overview)