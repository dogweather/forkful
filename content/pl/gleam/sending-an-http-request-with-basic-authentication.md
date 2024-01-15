---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnianiem"
html_title:           "Gleam: Wysyłanie żądania http z podstawowym uwierzytelnianiem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnianiem"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli chcesz wysłać zapytanie HTTP z uwierzytelnieniem podstawowym, jesteś we właściwym miejscu! W tym artykule dowieasz się, dlaczego ta funkcjonalność może być przydatna i jak jej używać w języku programowania Gleam.

## Jak to zrobić

Gleam oferuje prosty sposób na wysyłanie zapytań HTTP z uwierzytelnieniem podstawowym. Wystarczy użyć funkcji `http.request_with_basic_auth()` i podać do niej adres URL, nazwę użytkownika oraz hasło w odpowiednich argumentach.

```Gleam
let response = http.request_with_basic_auth("https://example.com", "username", "password")
```

Jeśli chcesz zobaczyć odpowiedź z serwera, możesz wyświetlić ją na ekranie lub zapisać do zmiennej i przetworzyć jej zawartość. Przykładowy kod można znaleźć poniżej:

```Gleam
let response = http.request_with_basic_auth("https://example.com", "username", "password")
let body = case response {
  Ok(resp) -> resp.body,
  Error(_) -> "Wystąpił błąd!",
}

io.println(body)
```

W powyższym przykładzie wykorzystaliśmy funkcję `io.println()` by wyświetlić zawartość odpowiedzi z serwera na ekranie. Możesz również przetworzyć tę zawartość na inne sposoby, na przykład parsując ją jako JSON lub zapisując do pliku.

## Głębsze spojrzenie

Wysyłanie zapytań HTTP z uwierzytelnieniem podstawowym jest przydatne w przypadkach, gdy chcesz uzyskać dostęp do zabezpieczonych zasobów, na przykład danych użytkownika lub prywatnych API. Dzięki Gleam możesz szybko i łatwo zaimplementować tę funkcjonalność w swoim kodzie.

Podczas wysyłania zapytania z uwierzytelnieniem podstawowym, Twój kod musi dostarczyć do serwera odpowiednie dane uwierzytelniające w nagłówku `Authorization`. Jeśli serwer wymaga informacji o nazwie użytkownika i haśle, nagłówek będzie wyglądać następująco:

```
Authorization: Basic <base64_encoded_username_and_password>
```

Możesz wygenerować odpowiedni base64 za pomocą modułu `base64` dostępnego w Gleam. Przykładowy kod:

```Gleam
import base64

let encoded = base64.encode("username:password")
io.println("Basic " <> encoded)
```

Teraz wiesz, dlaczego i jak wysyłać zapytania HTTP z uwierzytelnieniem podstawowym w Gleam. Nie zapomnij poeksperymentować z różnymi funkcjonalnościami Gleam, aby jeszcze lepiej poznać ten język!

## Zobacz też

- Dokumentacja Gleam: [https://gleam.run/](https://gleam.run/)
- Biblioteka standardowa Gleam: [https://gleam.run/modules/standard-library.html](https://gleam.run/modules/standard-library.html)
- Moduł `http`: [https://gleam.run/modules/http.html](https://gleam.run/modules/http.html)