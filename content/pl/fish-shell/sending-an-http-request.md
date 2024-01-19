---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Wysyłanie żądania HTTP to proces, w którym twój komputer prosi o dane z serwera internetowego. Programiści robią to, żeby komunikować się z zasobami internetowymi, takimi jak strony internetowe, API, czy bazy danych.

## Jak to zrobić:

Przykład jak wysłać żądanie GET używając curl w Fish Shell:

```Fish Shell
function wyslij_get
  set url $argv[1]
  curl $url
end
```

Teraz możemy wywołać tę funkcję z dowolnym adresem URL, np.:

```Fish Shell
> wyslij_get https://example.com
```

Przykład wyjścia mogłoby wyglądać tak:

```Fish Shell
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## Głębsze Zanurzenie 

Wysyłanie żądań HTTP było podstawową częścią internetu od połowy lat 90-tych. Przede wszystkim używane jest do pobierania zasobów z internetu, ale także do komunikacji z usługami API.

Jest wiele alternatyw dla curl, takie jak wget, httpie czy Postman. Każde narzędzie ma swoje unikalne cechy i przewagi.

Szczegóły implementacji wysyłania żądań HTTP mogą się różnić w zależności od narzędzia używanego do tego celu. W powyższym przykładzie, curl tworzy TCP połączenie do serwera określonego przez URL, a następnie wysyła żądanie HTTP.

## Zobacz również

HTTP: [https://pl.wikipedia.org/wiki/Hypertext_Transfer_Protocol](https://pl.wikipedia.org/wiki/Hypertext_Transfer_Protocol)

Curl: [https://curl.se/](https://curl.se/)

Fish Shell: [https://fishshell.com/](https://fishshell.com/)