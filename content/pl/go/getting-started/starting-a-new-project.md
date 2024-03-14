---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:39.405400-07:00
description: "Rozpocz\u0119cie nowego projektu w Go wi\u0105\u017Ce si\u0119 z przygotowaniem\
  \ obszaru roboczego i zainicjowaniem go przy pomocy niezb\u0119dnych modu\u0142\xF3\
  w Go. Programi\u015Bci robi\u0105 to\u2026"
lastmod: '2024-03-13T22:44:34.851711-06:00'
model: gpt-4-0125-preview
summary: "Rozpocz\u0119cie nowego projektu w Go wi\u0105\u017Ce si\u0119 z przygotowaniem\
  \ obszaru roboczego i zainicjowaniem go przy pomocy niezb\u0119dnych modu\u0142\xF3\
  w Go. Programi\u015Bci robi\u0105 to\u2026"
title: Rozpoczynanie nowego projektu
---

{{< edit_this_page >}}

## Co i dlaczego?

Rozpoczęcie nowego projektu w Go wiąże się z przygotowaniem obszaru roboczego i zainicjowaniem go przy pomocy niezbędnych modułów Go. Programiści robią to w celu organizacji kodu, skutecznego zarządzania zależnościami oraz ułatwienia procesów kompilacji. Jest to podstawowe dla tworzenia skalowalnych i łatwych w utrzymaniu oprogramowań w Go.

## Jak to zrobić:

Najpierw upewnij się, że Go jest zainstalowane, uruchamiając `go version` w terminalu. Powinieneś zobaczyć zainstalowaną wersję Go jako wynik. Następnie, zacznijmy nowy projekt. Przejdź do swojego obszaru roboczego i uruchom:

```shell
mkdir hello-world
cd hello-world
```

To tworzy i przenosi cię do nowego katalogu dla twojego projektu. Teraz zainicjuj moduł:

```shell
go mod init example.com/hello-world
```

Zamień `example.com/hello-world` na ścieżkę swojego modułu. To polecenie tworzy w twoim katalogu plik `go.mod`, sygnalizując początek nowego modułu Go. Oto jak może wyglądać `go.mod`:

```plaintext
module example.com/hello-world

go 1.18
```

`go.mod` śledzi zależności twojego projektu. Teraz utwórz plik `main.go`:

```shell
touch main.go
```

Otwórz `main.go` w swoim ulubionym edytorze i dodaj następujący kod, aby wydrukować "Hello, World!":

```go
package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
}
```

Aby uruchomić swój program, wróć do terminala i wykonaj:

```shell
go run main.go
```

Powinieneś zobaczyć:

```plaintext
Hello, World!
```

Gratulacje! Właśnie zacząłeś nowy projekt Go i uruchomiłeś swój pierwszy program w Go.

## Wnikliwe spojrzenie

Inicjatywa wprowadzenia modułów jako standardu do zarządzania zależnościami w Go była znaczącą zmianą w ekosystemie Go, oficjalnie przyjętą w Go 1.11. Przed modułami, programiści Go opierali się na zmiennej środowiskowej GOPATH do zarządzania zależnościami, co było mniej intuicyjne i często prowadziło do niesławnej "piekła zależności".

Moduły zapewniają enkapsulowany sposób zarządzania zależnościami projektu, wersjonowania i są krokiem w kierunku robienia projektów Go bardziej samowystarczalnymi i przenośnymi. Każdy moduł określa swoje zależności, które Go śledzi w pliku `go.mod`, upraszczając zarządzanie zależnościami w różnych środowiskach i etapach rozwoju.

Jednak warto zauważyć, że chociaż moduły Go są teraz standardem, niektóre starsze projekty mogą nadal używać GOPATH. Dla większości nowych projektów moduły oferują prostszy i skuteczniejszy system zarządzania, ale zrozumienie GOPATH może być przydatne dla utrzymania lub przyczynienia się do starszych baz kodów Go.

Jeśli chodzi o alternatywy, chociaż moduły Go są teraz de facto standardem, społeczność Go eksperymentowała w przeszłości z innymi narzędziami do zarządzania zależnościami, takimi jak `dep`. Jednak te zostały w dużej mierze zastąpione przez oficjalne wsparcie modułów zintegrowane z narzędziownią Go.
