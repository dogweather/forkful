---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Ruby: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Wysyłanie zapytania HTTP z podstawową autoryzacją może być przydatne w wielu sytuacjach, na przykład do pobrania danych zabezpieczonych hasłem lub autoryzowania dostępu do zasobów sieciowych.

## Jak To Zrobić

```Ruby
require 'net/http'

uri = URI('https://example.com')
username = 'user'
password = 'secret'

Net::HTTP.start(uri.host, uri.port, use_ssl: uri.scheme == "https") do |http|
  request = Net::HTTP::Get.new(uri)
  request.basic_auth(username, password)
  response = http.request(request)

  puts response.body
end
```

Po uruchomieniu tego kodu, w konsoli zostanie wyświetlona odpowiedź zasobu sieciowego, do którego się odwołujemy. Zwrócone dane będą zaszyfrowane za pomocą podstawowej autoryzacji, co oznacza, że tylko użytkownicy posiadający prawidłowe dane logowania będą w stanie je odczytać.

## Głębsza Analiza

Wysyłanie zapytania HTTP z podstawową autoryzacją odbywa się poprzez dodanie nagłówka `Authorization` do żądania. Nagłówek ten zawiera kodowanie Base64 nazwy użytkownika i hasła. Wysłany kod jest następnie weryfikowany przez serwer, a dostęp jest udostępniony tylko jeśli dane logowania są poprawne.

## Zobacz Również

- [Dokumentacja Ruby o podstawowej autoryzacji](https://ruby-doc.org/stdlib-2.7.2/libdoc/net/http/rdoc/Net/HTTP.html#class-Net::HTTP-label-Basic+Authentication)
- [Artykuł na temat podstawowej autoryzacji w HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme) (w języku angielskim)
- [Przykłady użycia podstawowej autoryzacji w aplikacjach internetowych](https://www.howtogeek.com/361361/what-is-http-authentication-and-how-does-it-work/) (w języku angielskim)