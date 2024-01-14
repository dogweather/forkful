---
title:                "Go: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Każdy internetowy użytkownik od czasu do czasu natknął się na potrzebę pobrania strony internetowej. Może to być konieczne, gdy chcemy zachować kopię strony lub przeczytać ją później offline. W tym artykule zobaczymy, jak za pomocą języka Go możemy łatwo pobrać dowolną stronę internetową.

## Jak to zrobić

Najpierw musimy zaimportować moduł "net/http", który umożliwi nam pobranie strony. Wystarczy wywołać funkcję "Get" z odpowiednim adresem URL i przypisać wynik do zmiennej. Następnie wykorzystując funkcję "ReadAll" z modułu "io/ioutil", możemy odczytać całą treść strony.

```Go
package main

import (
    "fmt"
    "io/ioutil"
    "net/http"
)

func main() {
    url := "https://example.com"
    resp, err := http.Get(url)
    if err != nil {
        fmt.Println("Wystąpił błąd:", err)
    }
    defer resp.Body.Close()
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        fmt.Println("Wystąpił błąd:", err)
    }
    fmt.Println(string(body))
}
```

Po uruchomieniu powyższego kodu, w konsoli zobaczymy cały kod HTML strony "https://example.com". Możemy również zapisywać ten kod do pliku lub wykorzystać go w dowolny inny sposób.

## Głębsze zagłębienie

Pobranie strony internetowej za pomocą języka Go jest możliwe dzięki wykorzystaniu biblioteki "net/http". Ta biblioteka zapewnia wiele funkcji, które umożliwiają interakcję z internetem, w tym również pobieranie stron. Warto również pamiętać, że w przypadku bardziej skomplikowanych stron, może być konieczne przetworzenie pobranego kodu HTML w celu odfiltrowania interesujących nas informacji.

## Zobacz także

- Oficjalna dokumentacja Go: https://golang.org/doc/
- Przewodnik po języku Go: https://github.com/robpike/Go-Language-Project

Dzięki wykorzystaniu języka Go, pobieranie stron internetowych staje się łatwe i szybkie. Wystarczy wykorzystać odpowiednie funkcje i biblioteki, aby uzyskać dostęp do interesujących nas informacji z internetu. Zachęcam do eksperymentowania i wykorzystywania tego języka w codziennej pracy.