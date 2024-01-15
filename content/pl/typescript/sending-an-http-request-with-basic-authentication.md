---
title:                "Wysyłanie żądania http z uwierzytelnieniem podstawowym"
html_title:           "TypeScript: Wysyłanie żądania http z uwierzytelnieniem podstawowym"
simple_title:         "Wysyłanie żądania http z uwierzytelnieniem podstawowym"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Wysyłanie żądania HTTP z uwierzytelnieniem podstawowym może być konieczne, gdy chcemy uzyskać dostęp do zasobów, które wymagają uwierzytelnienia. Jest to powszechna metoda używana w systemach autoryzacyjnych i może być niezbędna w niektórych przypadkach.

## Jak to zrobić

```TypeScript
import axios from "axios";

// Przykładowe zapytanie HTTP z uwierzytelnieniem podstawowym
axios.get("https://example.com/api/data", {
  auth: {
    username: "username",
    password: "password"
  }
})
.then(response => {
  // Przetwarzanie odpowiedzi z serwera
  console.log(response.data);
})
.catch(error => {
  // Obsługa błędu
  console.log(error);
});
```

Output:

```
{
  // Odpowiedź z serwera
  data: {
    // Dane zasobu
  }
}
```

## Deep Dive

Uwierzytelnienie podstawowe to metoda autoryzacji polegająca na przesyłaniu danych uwierzytelniających w nagłówku żądania HTTP. Jest to najprostsza i najmniej bezpieczna metoda uwierzytelniania, ponieważ dane uwierzytelniające są przesyłane w otwartym tekście bez szyfrowania.

W celu wysłania żądania z uwierzytelnieniem podstawowym, należy wstawić obiekt `auth` w drugim parametrze funkcji `axios.get()`. Obiekt ten musi zawierać właściwości `username` i `password`, które przechowują odpowiednie dane uwierzytelniające. Jeśli uwierzytelnienie jest prawidłowe, serwer zwróci odpowiedź w postaci obiektu `response`, który zawiera informacje o zasobie.

## Zobacz także
- [Dokumentacja Axios](https://github.com/axios/axios)
- [Tutorial uwierzytelniania podstawowego w TypeScript](https://www.digitalocean.com/community/tutorials/typescript-axios)
- [Wprowadzenie do uwierzytelniania w aplikacjach webowych](https://blog.bitsrc.io/a-beginners-guide-to-authentication-in-node-js-with-passport-js-1da3a0e1e4f8)