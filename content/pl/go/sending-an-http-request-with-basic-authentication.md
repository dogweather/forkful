---
title:                "Go: Wysyłanie żądania http z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego
W tym blogu przyjrzymy się, dlaczego korzystanie z uwierzytelniania podstawowego jest ważne podczas wysyłania żądań HTTP w języku Go.

## Jak to zrobić
```Go
package main

import (
    "bytes"
    "fmt"
    "net/http"
)

func main() {
    // Tworzenie requesta z uwierzytelnianiem pośrednikowym
    req, err := http.NewRequest("GET", "https://example.com", nil)
    if err != nil {
        panic(err)
    }

    // Dodawanie podstawowego uwierzytelnienia do nagłówka
    req.SetBasicAuth("username", "password")

    // Wysyłanie requesta
    client := http.Client{}
    res, err := client.Do(req)
    if err != nil {
        panic(err)
    }

    // Wypisywanie odpowiedzi
    var buf bytes.Buffer
    buf.ReadFrom(res.Body)
    fmt.Println(buf.String())
}
```

**Output:**
Koketktyvtv

## Głębsze spojrzenie
Podczas wysyłania żądania HTTP z uwierzytelnianiem podstawowym, kluczową kwestią jest użycie funkcji `http.NewRequest()` do utworzenia nowego requesta. Następnie, za pomocą metody `SetBasicAuth()` możemy dodać nazwę użytkownika i hasło do nagłówka. Warto również pamiętać, że uwierzytelnianie podstawowe nie jest zbyt bezpieczne, ponieważ hasło jest przesyłane bezpośrednio w nagłówku. Dlatego warto rozważyć inne metody uwierzytelniania, takie jak uwierzytelnianie tokenem.

## Zobacz również
- [Dokumentacja Go o wysyłaniu żądań HTTP](https://golang.org/pkg/net/http/)
- [Poradnik o uwierzytalnianiu podstawowym w języku Go](https://www.sohamkamani.com/golang/basic-http-authentication/)
- [Przewodnik po bezpieczeństwie uwierzytelniania HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)