---
title:                "Gleam: Wysyłanie żądania HTTP"
simple_title:         "Wysyłanie żądania HTTP"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego
Każdego dnia przeglądając internet, wchodzimy na różne strony internetowe, a także korzystamy z aplikacji mobilnych. Nie zastanawiamy się zazwyczaj nad procesem, który umożliwia nam otrzymywanie informacji z tych źródeł. Jednym z głównych sposobów na przesyłanie informacji z internetu jest wykorzystanie protokołu HTTP (Hypertext Transfer Protocol). W artykule dowiesz się, dlaczego i w jaki sposób możesz użyć języka programowania Gleam do wysyłania żądań HTTP.

## Jak to zrobić
Gleam posiada wbudowane metody, które umożliwiają wysłanie żądania HTTP w prosty sposób. Aby wysłać żądanie HTTP, należy najpierw zdefiniować URL, na który chcemy wysłać żądanie, a następnie ustawić opcje żądania, takie jak typ żądania, parametry i nagłówki. Poniżej znajduje się przykładowy kod w języku Gleam, który pokazuje, jak wysłać prosty żądanie GET i odczytać odpowiedź:

```Gleam
import gleam/http.{Request, Response}
import gleam/http.{get, ok_status}

// Definiowanie URL i opcji żądania
url = "https://www.example.com"
options = Request.options(get(), {headers})
headers = Request.headers({{"content-type", "application/json"}})

// Wysłanie żądania i odczytanie odpowiedzi
response = Request.get(url, options)
case response of
    ok_status(body) -> body
    error -> "Wystąpił błąd: $error"
```

Rezultatem powyższego kodu jest otrzymanie zawartości strony internetowej https://www.example.com w formie tekstu. Warto także pamiętać, że można zmieniać opcje żądania w celu bardziej zaawansowanego przetwarzania danych lub dostosowania do konkretnej sytuacji.

## Deep Dive
Wysłanie żądania HTTP jest procesem bardziej skomplikowanym niż tylko zdefiniowanie URL i wywołanie odpowiedniej funkcji. W głębszym zanurzeniu możemy zrozumieć, że w procesie wysyłania żądania HTTP zachodzą różne etapy, takie jak nawiązywanie połączenia z serwerem, przetwarzanie nagłówków i ciała odpowiedzi, a także obsługa błędów. W języku Gleam istnieje wiele funkcji i modułów, które umożliwiają nam zarządzanie całym procesem wysyłania żądań HTTP w bardziej zaawansowany sposób.

## Zobacz też
- [Dokumentacja HTTP w języku Gleam] (https://gleam.run/documentation/http)
- [Podstawy obsługi żądań HTTP w języku Gleam] (https://dev.to/gleam_language/getting-started-with-http-in-gleam-8l0)
- [Przykładowy kod do wysyłania żądań HTTP w języku Gleam] (https://gist.github.com/gleam-lang/ae47780461b65c823ec9191774d593fc)