---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Pobieranie strony internetowej to technika pozyskiwania i zapisywania danych z określonego URL. Programiści robią to, aby przetworzyć, analizować lub wykorzystać treści stron internetowych w swoich projektach.

## Jak to zrobić:

Aby pobrać stronę internetową za pomocą języka programowania Ruby, możemy użyć biblioteki `open-uri`. Oto prosty kod, który to robi:

```ruby
require 'open-uri'

url = 'http://example.com'

open(url) do |webpage|
  File.open('my_local_file.html', 'w') do |file|
    file.write(webpage.read)
  end
end
```

Jako wynik, dane ze strony `http://example.com` zostaną pobrane i zapisane lokalnie w pliku `my_local_file.html`.

## Pogłębienie:

Pobieranie stron internetowych ma swoje korzenie w początkach internetu, kiedy HTML był jedynym językiem odpowiedzialnym za wygląd i zawartość strony. Przy użyciu niewielkiego fragmentu kodu, programiści mogą pobierać i analizować te dane, jakkolwiek chcą.

Warto jednak pamiętać o innym popularnym narzędziu jakim jest `Net::HTTP` z biblioteki standardowej Ruby.

```ruby
require 'net/http'

url = 'http://example.com'
response = Net::HTTP.get_response(URI(url))

File.open('my_second_local_file.html', 'w') do |file|
  file.write(response.body)
end
```

Wiadomość `HTTP GET` jest wysyłana do serwera, a korpus odpowiedzi (stronę internetową) zapisujemy w pliku. Metoda `Net::HTTP#get_response` jest bardziej skomplikowana, ale daje nam pełną kontrolę nad żądaniami i odpowiedziami HTTP, co jest przydatne w przypadku bardziej złożonych operacji.

## Zobacz także:

1. Dokumentacja open-uri: [https://www.rubydoc.info/stdlib/open-uri](https://www.rubydoc.info/stdlib/open-uri)

2. Dokumentacja Net::HTTP: [https://ruby-doc.org/stdlib-3.0.1/libdoc/net/http/rdoc/Net/HTTP.html](https://ruby-doc.org/stdlib-3.0.1/libdoc/net/http/rdoc/Net/HTTP.html)

3. Artykuł o przetwarzaniu HTML w Ruby: [https://nokogiri.org](https://nokogiri.org)