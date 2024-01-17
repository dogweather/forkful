---
title:                "Wysyłanie żądania http"
html_title:           "Ruby: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP jest ważną częścią tworzenia aplikacji internetowych. Jest to prosty sposób na pobieranie danych z innych stron internetowych lub serwerów. Programiści wykorzystują tę technikę do integracji swoich aplikacji z innymi serwisami oraz do pobierania danych z różnych źródeł.

## Jak to zrobić:

```ruby
require 'net/http' #importowanie biblioteki Net::HTTP
url = URI('https://example.com') #ustawienie adresu URL
http = Net::HTTP.new(url.host, url.port) #tworzenie obiektu HTTP
http.use_ssl = true #włączenie obsługi HTTPS
request = Net::HTTP::Get.new(url) #tworzenie obiektu żądania GET
response = http.request(request) #wysłanie żądania i otrzymanie odpowiedzi
puts response.body #wyświetlenie zawartości odpowiedzi
```

Przykładowy wynik:

```html
<!DOCTYPE html>
<html>
  <head>
    <title>Example Domain</title>
  </head>
  <body>
    <h1>Example Domain</h1>
    <p>This domain is for use in illustrative examples in documents.</p>
    <p><a href="https://example.com/">More information...</a></p>
  </body>
</html>
```

## Głębszy zanurzenie:

Wysyłanie żądań HTTP jest jedną z podstawowych funkcji internetowych. Zostało wprowadzone w 1989 roku przez Tima Bernersa-Lee i stanowi podstawę komunikacji między przeglądarkami a serwerami. Alternatywnym podejściem do pobierania danych jest korzystanie z protokołu FTP, jednak możliwości HTTP są znacznie szersze i lepiej dostosowane do potrzeb współczesnych aplikacji internetowych.

W Ruby do wysyłania żądań HTTP wykorzystuje się bibliotekę Net::HTTP, jednak istnieją również inne biblioteki, takie jak Faraday czy HTTParty, które mogą ułatwić proces i zapewnić dodatkowe funkcje.

Implementacja żądania GET jest jednym z najprostszych sposobów na pobranie danych z serwera. Jednak istnieją też inne rodzaje żądań, takie jak POST, PUT czy DELETE, które są wykorzystywane do zapisywania, aktualizacji lub usuwania danych.

## Zobacz również:

- [Dokumentacja biblioteki Net::HTTP] (https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
- [Tutorial "HTTP z Ruby" na Medium] (https://medium.com/@dam1891/practical-introduction-to-http-with-ruby-efda7e3e1a77)
- [Porównanie różnych bibliotek HTTP w Ruby] (https://realpython.com/build-python-web-apps-with-httpx/)