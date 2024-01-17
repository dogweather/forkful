---
title:                "Wysyłanie żądania http z podstawową autoryzacją"
html_title:           "Swift: Wysyłanie żądania http z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wysyłanie żądania HTTP z podstawową autoryzacją to jedna z podstawowych czynności, które programiści wykonują podczas tworzenia aplikacji internetowych. Jest to sposób na uwierzytelnienie użytkowników poprzez przesyłanie danych w formacie klucz-wartość. Programiści wykorzystują tę metodę, ponieważ jest prosta w implementacji i zapewnia podstawowe zabezpieczenia dostępu do danych.

## Jak to zrobić:
```Swift
let username = "myUsername"
let password = "myPassword"
let loginString = "\(username):\(password)"
let loginData = loginString.data(using: .utf8)
if let base64LoginData = loginData?.base64EncodedString() {
    // Stwórz wywołanie HTTP, dodając nagłówek `Authorization: Basic base64LoginData` do żądania
}
```

## Głębsze zanurzenie:
Metoda autoryzacji z podstawowym logowaniem była używana od początku istnienia HTTP, ale obecnie zaleca się unikanie jej na rzecz bezpieczniejszych metod, takich jak autoryzacja tokenów lub OAuth. Jednak nadal jest szeroko stosowana, ponieważ jest łatwa do zaimplementowania i często wystarczająca dla prostych aplikacji. W przypadku bardziej bezpiecznych zastosowań, programiści mogą wykorzystać protokół HTTPS w połączeniu z podstawową autoryzacją, aby zapewnić lepszą ochronę danych.

## Zobacz także:
- Dokumentacja Apple na temat klasy `URLSession` i możliwości dodawania nagłówka autoryzacji do żądań HTTP: https://developer.apple.com/documentation/foundation/urlsession
- Artykuł na stronie Swift by Sundell o zabezpieczaniu żądań HTTP z podstawową autoryzacją: https://www.swiftbysundell.com/articles/securizing-network-requests-in-swift/
- Oficjalna specyfikacja protokołu HTTP wraz z opisem nagłówka autoryzacji: https://tools.ietf.org/html/rfc1945#section-11.3