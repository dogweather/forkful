---
title:                "Go: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

W programowaniu, często istnieje potrzeba przekazania pewnych danych w trakcie uruchamiania naszej aplikacji. W języku Go, istnieje łatwy i szybki sposób na odczytanie tych danych z lini poleceń uruchomieniowej. W tym wpisie dowiesz się dokładnie dlaczego warto korzystać z odczytywania argumentów z linii poleceń w programowaniu w języku Go.

## Jak używać

Odczytanie argumentów z linii poleceń jest bardzo proste i odbywa się przy użyciu funkcji `os.Args`. Ta funkcja zwraca tablicę argumentów podanych w lini poleceń. W poniższym przykładzie wykorzystamy tą funkcję do wyświetlenia wszystkich podanych w lini poleceń argumentów:

```Go
package main

import "fmt"
import "os"

func main() {
    args := os.Args
    fmt.Println("Podane argumenty: ", args)
}
```

Przykładowe wywołanie naszego programu z lini poleceń może wyglądać następująco:

```
go run main.go argument_1 argument_2 argument_3
```

Zostanie wówczas wyświetlony następujący wynik:

```
Podane argumenty: [main argument_1 argument_2 argument_3]
```

Dzięki temu prostemu sposobowi, możemy bardzo łatwo przekazać różne dane do naszej aplikacji.

## Głębszy zanurzenie

W języku Go, istnieje również wbudowany pakiet `flag`, który pozwala na jeszcze prostsze i bardziej przejrzyste odczytywanie argumentów z linii poleceń. Przy użyciu tego pakietu, możemy definiować różne flagi, które pozwalają na przekazanie wartości do naszej aplikacji. Jest to bardzo przydatne, gdy chcemy przekazać opcjonalne parametry, np. ustawienia portu czy ścieżki do pliku. Poniżej przedstawiamy przykład użycia pakietu `flag`:

```Go
package main

import "fmt"
import "flag"

func main() {

    // Definiowanie flagi - arg to nazwa flagi, a "wartość domyślna" jest wartością domyślną
    arg := flag.String("arg", "wartość domyślna", "opis flagi")

    // Wymagane wywołanie funkcji parsującej flagi
    flag.Parse()

    // Wyświetlenie wartości przekazanej w argumencie
    fmt.Println("Podana flaga \"arg\":", *arg)
}
```

Wywołanie tego programu z opcjonalnym argumentem o nazwie "flaga" i wartością "wartość" wyglądałoby w ten sposób:

```
go run main.go -arg flaga
```

W wyniku zostanie wyświetlony następujący wynik:

```
Podana flaga "arg": flaga
```

Oczywiście, jest to tylko mały wycinek możliwości pakietu `flag`. Można go wykorzystać na wiele różnych sposobów, w zależności od naszych potrzeb.

## Zobacz także

Zapoznaj się z dokumentacją języka Go oraz pakietu `flag`, aby jeszcze bardziej zgłębić swoją wiedzę na temat odczytywania argumentów z linii poleceń w tym języku programowania:

- [Dokumentacja Go](https://golang.org/doc/)
- [Dokumentacja pakietu flag](https://golang.org/pkg/flag/)

Dziękujemy za przeczytanie tego wpisu! Mamy nadzieję, że pomoże Ci on w przyszłych projektach.