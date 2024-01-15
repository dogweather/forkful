---
title:                "Pobieranie strony internetowej"
html_title:           "Ruby: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobieranie treści strony internetowej może być niezwykle przydatne w wielu sytuacjach, na przykład podczas tworzenia scraperów lub analizowania danych.

## Jak

```Ruby
require 'net/http'

url = URI.parse('www.example.com')
http = Net::HTTP.new(url.host, url.port)
request = Net::HTTP::Get.new(url)
response = http.request(request)
puts response.read_body
```

Ten prosty kod wykorzystuje bibliotekę Net::HTTP do pobrania treści strony internetowej za pomocą protokołu HTTP. W kodzie używamy adresu URL strony, tworzymy obiekt zapytania HTTP, wysyłamy je za pomocą obiektu HTTP i odczytujemy odpowiedź, aby wyświetlić treść strony.

## Dogłębna analiza

Funkcja Net::HTTP::Get pozwala na wykonanie zapytania HTTP metodą GET, co oznacza pobranie treści z danej strony. Oprócz tego, Ruby ma również wiele innych bibliotek umożliwiających pobieranie i przetwarzanie treści z różnych źródeł.

## Zobacz także

- Oficjalna dokumentacja Ruby: https://www.ruby-lang.org/pl/documentation/
- Dokumentacja biblioteki Net::HTTP: https://docs.ruby-lang.org/en/2.6.0/Net/HTTP.html
- Przykładowy projekt scrapera w Ruby: https://github.com/jsejtko/scraper-demo