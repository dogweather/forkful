---
title:                "Ruby: Wysyłanie żądania HTTP z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania HTTP z podstawową autoryzacją"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego wysyłamy żądanie HTTP z podstawową autoryzacją? Istnieje wiele powodów, dla których może się to okazać niezbędne w programowaniu. Jednym z nich może być uzyskanie dostępu do chronionego API, lub zabezpieczenie danych przed nieautoryzowanym dostępem.

## Jak to zrobić

```Ruby
require 'net/http'
require 'uri'

uri = URI.parse("url-do-api")
http = Net::HTTP.new(uri.host, uri.port)
request = Net::HTTP::Get.new(uri.request_uri)
request.basic_auth("login", "hasło")

response = http.request(request)

puts response.body

```

**Wynik:**
```
Pomyślnie otrzymano dane z API.
```

W powyższym przykładzie korzystamy z biblioteki `net/http` i klasy `Net::HTTP` do utworzenia żądania GET do API z wykorzystaniem podstawowej autoryzacji. Za pomocą metody `basic_auth` przekazujemy login i hasło, a następnie wykonujemy żądanie i wyświetlamy otrzymane dane.

## Głębszy kontekst

Podstawowa autoryzacja HTTP jest jednym z najprostszych sposobów na zabezpieczenie dostępu do danych lub API. Polega ona na przesyłaniu danych uwierzytelniających w nagłówku żądania HTTP, w postaci loginu i hasła, połączonych dwukropkiem i zakodowanych do formatu Base64.

Przykładowy nagłówek z podstawową autoryzacją może wyglądać tak:
```
Authorization: Basic YWRtaW46cGFzc3dvcmQ=
```

Jednak ta metoda nie jest bezpieczna, ponieważ hasło jest przesyłane w formie tekstu jawnej i może zostać przechwycone przez osoby niepowołane. Dlatego zaleca się stosowanie bardziej zaawansowanych metod autoryzacji, takich jak OAuth czy Token-based authentication.

## Zobacz także

- [Dokumentacja biblioteki Net::HTTP w języku polskim](https://ruby-doc.org/stdlib-2.7.2/libdoc/net/http/rdoc/Net/HTTP.html)
- [Inne metody autoryzacji w API](https://www.codecademy.com/articles/what-is-basic-auth)