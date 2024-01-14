---
title:                "Go: Rozpoczynając nowy projekt"
simple_title:         "Rozpoczynając nowy projekt"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Dlaczego

Rozpoczęcie nowego projektu jest ważną częścią nauki języka Go. Pozwala ono na praktykowanie podstawowych umiejętności i poszerzanie wiedzy o programowaniu w tym języku. Ponadto, tworzenie własnych projektów może być również świetnym sposobem na wykorzystanie kreatywności i rozwoju umiejętności problem solving.

## Jak zacząć

Aby rozpocząć nowy projekt w języku Go, wystarczy utworzyć nowy plik z rozszerzeniem `.go` oraz zainicjować moduł przez wpisanie w terminalu komendy `go mod init nazwa_projektu`. Następnie, można zacząć pisać kod wewnątrz pliku, wykorzystując różne pakiety oraz funkcje dostępne w języku Go. Poniżej znajduje się przykładowy kod, który wyświetli napis "Witaj świecie!" na ekranie:

```Go
package main

import "fmt"

func main() {
    fmt.Println("Witaj świecie!")
}
```

Można również wykorzystać zewnętrzne biblioteki, korzystając z menadżera modułów `go get` oraz importując je w kodzie. Należy jednak pamiętać, aby sprawdzać licencję oraz popularność danej biblioteki, aby uniknąć problemów w przyszłości.

## Deep Dive

Rozpoczęcie nowego projektu w języku Go może być nieco przytłaczające dla początkujących. Dlatego warto zacząć od prostych projektów, aby opanować podstawy języka oraz strukturę plików i pakietów. Następnie, można stopniowo przechodzić do bardziej zaawansowanych projektów i wykorzystywać różne funkcje i pakiety dostępne w języku.

Warto również pamiętać o testowaniu kodu oraz dokumentowaniu go poprzez wykorzystanie komentarzy oraz narzędzi takich jak `godoc`.

## Zobacz także

- [Oficjalna dokumentacja języka Go](https://golang.org/doc/)
- [Lista popularnych bibliotek w języku Go](https://github.com/avelino/awesome-go)
- [Kurs programowania w języku Go (w języku polskim)](http://www.golang-book.com/books/intro)