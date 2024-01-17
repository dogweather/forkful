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

## W czym rzecz i dlaczego?

Pobieranie strony internetowej to proces pobierania danych z internetu za pomocą kodu programistycznego. Programiści często wykonują tę czynność w celu pobrania treści ze strony i przetworzenia jej w sposób, który ułatwi dalsze działania.

## Jak to zrobić?

Aby pobrać stronę internetową w języku Ruby, możemy użyć modułu Net::HTTP. Najpierw musimy go załadować za pomocą ```require 'net/http'```. Następnie tworzymy obiekt klasy URI, aby określić adres URL strony, którą chcemy pobrać. Odwołujemy się do strony, wywołując metodę GET na obiekcie Net::HTTP. Otrzymamy wtedy obiekt odpowiedzi, który możemy przetworzyć i wyświetlić w konsoli. Przykładowy kod będzie wyglądał następująco:

```Ruby
require 'net/http'
url = URI.parse('https://www.example.com')
response = Net::HTTP.get_response(url)
puts response.body
```

Po uruchomieniu powyższego kodu, powinniśmy zobaczyć w konsoli kod HTML strony www.example.com.

## Głębsze spojrzenie

Pobieranie stron internetowych jest często wykorzystywane w wielu aplikacjach internetowych, takich jak web scraping czy automated testing. Istnieje wiele alternatywnych metod pobierania stron internetowych w języku Ruby, takich jak mechanize czy open-uri. W przypadku pobierania większych ilości danych z internetu, warto jednak rozważyć użycie biblioteki Nokogiri, która pozwala na łatwe przetwarzanie pobranej treści w formacie HTML lub XML.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o pobieraniu stron internetowych w języku Ruby, polecamy zapoznanie się z dokumentacją modułu Net::HTTP oraz biblioteki Nokogiri. Możesz także poszukać gotowych rozwiązań w postaci gemów, takich jak mechanize czy nokogiri.