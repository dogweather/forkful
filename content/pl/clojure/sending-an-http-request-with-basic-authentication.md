---
title:                "Przesyłanie żądania http z podstawową autoryzacją"
html_title:           "Clojure: Przesyłanie żądania http z podstawową autoryzacją"
simple_title:         "Przesyłanie żądania http z podstawową autoryzacją"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Wysyłanie żądania HTTP z uwierzytelnieniem podstawowym jest ważne, ponieważ umożliwia bezpieczny dostęp do zasobów w Internecie. Użycie uwierzytelnienia podstawowego pozwala użytkownikom potwierdzić swoją tożsamość przed uzyskaniem dostępu do chronionych danych.

## Jak to zrobić

```Clojure
(require '[org.httpkit.client :as http])
(http/post "https://example.com/api/resource" {:basic-auth ["username" "password"]})
```

Kod powyżej pokazuje przykład wysłania żądania POST z uwierzytelnieniem podstawowym do zasobu API na stronie internetowej. W celu użycia uwierzytelnienia podstawowego, należy przekazać wektor z nazwą użytkownika i hasłem jako drugi argument do funkcji `post`.

Output:

```Clojure
{:status 200, :headers {...}, :body "..."}
```

Gdy serwer odpowiedział na żądanie, otrzymamy obiekt z odpowiedzią zawierającym informacje o statusie odpowiedzi, nagłówkach i ciele odpowiedzi.

## Deep Dive

W przypadku wysyłania żądania z uwierzytelnieniem podstawowym, należy pamiętać, że dane uwierzytelniające są przesyłane w formacie Base64. Dzięki temu, że dane są kodowane, są one bezpieczniejsze w transmisji i mogą być odczytane tylko przez autoryzowaną stronę.

Istnieje również możliwość dodania uwierzytelnienia podstawowego do żądań GET, DELETE i PUT poprzez ustawienie odpowiednich kluczy w parametrze `:query-params` funkcji `http/get`, `http/delete` lub `http/put`.

## Zobacz też

- [Dokumentacja Clojure dotycząca uwierzytelnienia HTTP](https://clojure.github.io/http-kit/#http-kit.client-auth/broadcast-request-cors-interactive)
- [RFC 2617 dotyczące uwierzytelniania HTTP](https://tools.ietf.org/html/rfc2617)