---
title:                "Wysyłanie żądania http z podstawową autoryzacją"
html_title:           "Gleam: Wysyłanie żądania http z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Co & Dlaczego?

Wysyłanie żądania HTTP z podstawową autoryzacją to sposób dla programistów na uwierzytelnienie ich żądań do serwera. Jest to istotne w przypadku, gdy chcemy uzyskać dostęp do chronionego zasobu lub danych. Autoryzacja podstawowa to najprostsza metoda uwierzytelniania przy użyciu nazwy użytkownika i hasła.

Jak to zrobić:

```Gleam
import gleam/http
http.basic_auth("username", "password", http.post("https://example.com", { "data": "Hello World!" })
|> http.send
|> http.parse_json
```

Wywołanie funkcji `basic_auth` pozwala nam podać wymagane dane uwierzytelniające oraz żądanie, które chcemy wysłać. Możemy również określić inne ustawienia, takie jak metoda żądania, nagłówki i dane.

W głębszym zanurzeniu:

Wysyłanie żądania HTTP z podstawową autoryzacją jest szeroko stosowane w programowaniu, ponieważ umożliwia dostęp do chronionych zasobów i wymianę informacji z serwerami, które wymagają uwierzytelnienia. Istnieje również wiele innych metod uwierzytelniania, takich jak autoryzacja tokenami, ale autoryzacja podstawowa jest jedną z najprostszych i najbardziej popularnych.

W `gleam/http` znajduje się wiele przydatnych funkcji do pracy z żądaniami HTTP, w tym obsługa różnych typów uwierzytelniania. Wysyłanie żądania z podstawową autoryzacją odbywa się poprzez dodanie nagłówka `Authorization` z zakodowanym użytkownikiem i hasłem. Dodatkowe informacje można znaleźć w specyfikacji HTTP oraz dokumentacji `gleam/http`.

Zobacz także:

- Specyfikacja HTTP: https://tools.ietf.org/html/rfc2617
- Dokumentacja gleam/http: https://hexdocs.pm/gleam/Http.html