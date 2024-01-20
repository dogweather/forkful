---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?
Wysyłanie żądania HTTP to proces, w którym klient (np. przeglądarka) prosi o dane od serwera. Programiści robią to, aby pobierać, wysyłać lub aktualizować dane na serwerach - to jest podstawą wszelkiej interakcji sieciowej.

## Jak to zrobić?
Oto przykład kodu w Gleam, który pokazuje, jak wysłać żądanie HTTP:

```Gleam
  import gleam/http.{Request}
  
  let url = http.Uri.parse("https://example.com")
  let request = Request.get(url)
  let result = http.send(request)
```

Po uruchomieniu powyższego kodu, Gleam wyśle żądanie GET do "https://example.com" i zwróci wynik.

## Pogłębienie tematu
1. W przeszłości, żądania HTTP były wysyłane przy użyciu surowego tekstu, co było niewydajne i podatne na błędy. Dzisiaj, języki takie jak Gleam gwarantują, że żądanie jest prawidłowo sformatowane i bezpieczne.

2. Alternatywami dla `http.send` w Gleam są różne biblioteki HTTP dostępne w innych językach, takie jak `reqwest` w Rust, `http.client` w Python, lub `axios` w JavaScript.

3. Wysyłanie żądania HTTP to nie tylko wysyłanie tekstu do serwera. Za kulisami, Gleam musi nawiązać bezpieczne połączenie z serwerem, odpowiednio sformatować żądanie, a następnie przetworzyć odpowiedź serwera.

## Zobacz też
2. Szczegółowe informacje o protokole HTTP: [link](https://developer.mozilla.org/pl/docs/Web/HTTP)
3. Biblioteki HTTP w innych językach: [reqwest](https://docs.rs/reqwest/), [http.client](https://docs.python.org/3/library/http.client.html), [axios](https://www.npmjs.com/package/axios)