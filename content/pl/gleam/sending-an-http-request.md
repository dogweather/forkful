---
title:                "Wysyłanie żądania http"
html_title:           "Gleam: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

Wysyłanie żądania HTTP jest niezbędnym elementem w programowaniu aplikacji internetowych. Jest to jedna z podstawowych czynności, których musimy się nauczyć, aby móc korzystać z danych zdalnych lub innego serwisu.

## Jak to zrobić

W celu wysłania żądania HTTP w Gleam, musimy użyć funkcji `http.send()`. Poniżej znajduje się przykładowy kod, który wysyła żądanie GET do strony internetowej i drukuje odpowiedź w konsoli.

```Gleam
let response = http.send(Request.get("https://example.com"))
console.log(response.body)
```

Wywołanie funkcji `http.send()` zwraca obiekt `Response`, który zawiera informacje o odpowiedzi serwera, w tym status, nagłówki i ciało odpowiedzi.

## Wnikliwe zagłębienie

Podczas wysyłania żądania HTTP w Gleam, możemy również ustawić opcje, takie jak nagłówki, parametry lub ciało żądania. Możemy również wybrać metodę wysyłania, na przykład `GET`, `POST` lub `PUT`. Wszystkie dostępne opcje są szczegółowo opisane w oficjalnej dokumentacji Gleam.

## Zobacz także

- Dokumentacja Gleam o wysyłaniu żądania HTTP: https://gleam.run/articles/http-client
- Przykładowy kod z wykorzystaniem biblioteki HTTP w Gleam: https://github.com/gleam-lang/awesome-gleam#client-side-http-requests