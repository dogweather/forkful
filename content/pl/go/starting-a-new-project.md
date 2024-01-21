---
title:                "Rozpoczynanie nowego projektu"
date:                  2024-01-20T18:03:35.553060-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Zaczynając nowy projekt w Go, tworzysz kosztownicę dla swojego kodu. Programiści robią to, by nadać strukturę, zarządzanie zależnościami i łatwość późniejszej pracy.

## How to: (Jak to zrobić:)
Tworzymy katalog projektu i inicjujemy moduł Go:

```Go
// W terminalu:
mkdir myproject
cd myproject
go mod init myproject
```

Twój pierwszy program:

```Go
package main

import "fmt"

func main() {
    fmt.Println("Witamy w nowym projekcie Go!")
}

// Uruchomienie programu:
// go run main.go

// Spodziewane wyjście:
// Witamy w nowym projekcie Go!
```

## Deep Dive (Głębsze zanurzenie)
W Go projekt rozpoczyna się od utworzenia folderu i zainicjowania modułu za pomocą `go mod`. To zastąpiło starsze `GOPATH` i `go get` dla lepszego zarządzania zależnościami. Moduły pojawiające się w Go 1.11 (2018) stały się standardem. Alternatywą są inne systemy budowania i zarządzania zależnościami, takie jak `dep`, ale `go mod` dominuje i jest zalecaną metodą. Implementacja modułów w Go upraszcza wersjonowanie, dzielenie się kodem i automatyzację budowy binarnej.

## See Also (Zobacz również)
- Dokumentacja modułów Go: https://golang.org/ref/mod
- Skuteczne Go: https://golang.org/doc/effective_go
- Tutorial "Rozpocznijmy przygodę z Go": https://golang.org/doc/tutorial/getting-started