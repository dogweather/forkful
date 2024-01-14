---
title:                "Gleam: Wysyłanie żądania http z podstawowym uwierzytelnianiem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnianiem"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Wysyłanie żądania HTTP z uwierzytelnieniem podstawowym jest kluczowym elementem tworzenia aplikacji internetowych. Pozwala ono na bezpieczną i poufną wymianę informacji z serwerem, co jest niezwykle ważne w dzisiejszych czasach. Dzięki temu, że żądania są uwierzytelnione, możemy mieć pewność, że tylko uprawnieni użytkownicy mają dostęp do naszych danych lub usług.

## Jak to zrobić

Aby wysłać żądanie HTTP z uwierzytelnieniem podstawowym w języku Gleam, musimy najpierw zaimportować bibliotekę `httpc`, która pozwala nam na wysyłanie żądań HTTP. Następnie, korzystając z funkcji `httpc.send`, możemy utworzyć żądanie z odpowiednimi nagłówkami uwierzytelniającymi.

```
Gleam import httpc

let request = httpc.send(
  method: "GET",
  url: "https://exampleapi.com/users/123",
  headers: [
    "Authorization" => "Basic dXNlcm5hbWU6cGFzc3dvcmRz"
  ]
)
```

W powyższym przykładzie, wysyłamy żądanie `GET` na adres URL https://exampleapi.com/users/123 z nagłówkiem uwierzytelniającym, zawierającym zakodowane dane logowania. W odpowiedzi, otrzymamy dane użytkownika o identyfikatorze 123.

Bardziej rozbudowany przykład możemy znaleźć w [dokumentacji biblioteki httpc](https://hexdocs.pm/gleam/Httpc.html).

## Głębsza analiza

Wysyłając żądania HTTP z uwierzytelnieniem podstawowym, należy pamiętać o kilku ważnych aspektach. Po pierwsze, dane uwierzytelniające powinny być zawsze wysyłane w formie zakodowanej, przy użyciu np. funkcji `base64.encode` w języku Gleam. Po drugie, wybrane metody uwierzytelniania (np. Basic lub Digest) muszą być zgodne z wymaganiami serwera. Warto również sprawdzić, czy żądanie zostało pomyślnie uwierzytelnione, analizując odpowiedni kod odpowiedzi HTTP.

## Zobacz również

- [Dokumentacja biblioteki httpc](https://hexdocs.pm/gleam/Httpc.html)
- [Dokumentacja funkcji base64.encode](https://hexdocs.pm/gleam/Base64.html#encode/1)
- [Przykładowa implementacja uwierzytelniania Basic w języku Gleam](https://gist.github.com/examplebasicauth)