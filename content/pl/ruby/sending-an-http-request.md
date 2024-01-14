---
title:                "Ruby: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach, korzystając z sieci, jesteśmy często zmuszeni do zwracania się do innych serwerów w celu pobrania danych lub wykonania określonych akcji. W takich sytuacjach, wysyłka żądania HTTP jest nieodłącznym elementem tworzenia programów. W tym blogu omówimy, dlaczego jest to ważny aspekt programowania i jak to zrobić w języku Ruby.

## Jak to zrobić

Aby wysłać żądanie HTTP w Ruby, możemy skorzystać z wbudowanej biblioteki `net/http`. Najpierw musimy utworzyć instancję `Net::HTTP` i przekazać do niej adres URL oraz opcję żądania (np. `get` lub `post`). Następnie należy wywołać metodę `request`, gdzie podajemy scieżkę i opcje zapytania. Oto przykładowy kod:

```Ruby
require 'net/http'

url = URI('https://example.com')
http = Net::HTTP.new(url.host, url.port)
request = Net::HTTP::Get.new(url)
response = http.request(request)

puts response.code
puts response.body
```

W powyższym przykładzie, możemy zobaczyć, że używamy metody `request` do wysłania żądania i zwrócenia odpowiedzi. Po otrzymaniu odpowiedzi, możemy wyświetlić jej kod oraz zawartość. Jeśli chcemy wysłać inne rodzaje żądań, możemy użyć innych opcji, takich jak `delete` lub `post`.

## Głębokie zagłębianie się

Wysyłka żądania HTTP może być czasami skomplikowana, szczególnie kiedy mamy do czynienia z bardziej skomplikowanymi przypadkami, takimi jak uwierzytelnianie lub obsługa błędów. W takich sytuacjach, używanie biblioteki `net/http` może być mniej wygodne, dlatego warto rozważyć użycie specjalnej biblioteki, takiej jak `httparty` lub `faraday`.

Podczas korzystania z tych bibliotek, mamy do dyspozycji różne metody, które ułatwiają wysyłkę żądań HTTP i obsługę odpowiedzi. Możemy także przekazać do nich odpowiednie opcje i konfiguracje, co dodatkowo ułatwia pracę z sieciami.

## Zobacz także

- [Dokumentacja biblioteki `net/http`](https://ruby-doc.org/stdlib-2.6.5/libdoc/net/http/rdoc/Net/HTTP.html)
- [Dokumentacja biblioteki `httparty`](https://github.com/jnunemaker/httparty)
- [Dokumentacja biblioteki `faraday`](https://lostisland.github.io/faraday/)