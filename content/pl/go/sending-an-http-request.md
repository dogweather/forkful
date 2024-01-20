---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP to proces, w którym komputer wysyła żądanie do serwera za pomocą protokołu HTTP. Programiści robią to, aby nawiązać komunikację między klientem a serwerem i przetwarzać dane.

## Jak to zrobić:

Poniżej znajduje się przykładowy kod Go, który wysyła żądanie GET do określonego URL.

```Go
package main

import (
    "fmt"
    "net/http"
    "io/ioutil"
)

func main() {
    response, err := http.Get("http://example.com")
    if err != nil {
        fmt.Printf("The HTTP request failed with error %s\n", err)
    } else {
        data, _ := ioutil.ReadAll(response.Body)
        fmt.Println(string(data))
    }
}
```

Gdy uruchomisz ten kod, zobaczysz odpowiedź HTML z powyższego URL.

## Deep Dive:

Wysyłanie żądań HTTP, jest praktykowane od wprowadzenia protokołu HTTP w 1991 r., co było rewolucją w sposobie, w jaki dane są przesyłane przez internet. 

W języku Go można użyć pakietu "net/http" do wysyłania żądań HTTP, ale istnieją również inne biblioteki, takie jak "fasthttp" i "goreq", które oferują więcej funkcji i są wydajniejsze pod różnymi względami.

W przeciwieństwie do niektórych innych języków, Go obsługuje żądania HTTP na niskim poziomie, co oznacza, że musisz obsługiwać wiele szczegółów samodzielnie. To jednak daje większą kontrolę nad tym, jak Twoje żądania są obsługiwane i przetwarzane.

## Zobacz też:

- Dokumentacja Go na temat pakietu net/http: https://golang.org/pkg/net/http/
- Porównanie różnych bibliotek HTTP dla Go: https://medium.com/@jenchikito/http-requests-in-golang-57e8a2721cbb
- Wprowadzenie do protokołu HTTP: https://developer.mozilla.org/pl/docs/Web/HTTP/Overview