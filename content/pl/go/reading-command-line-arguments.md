---
title:    "Go: Odczytywanie argumentów wiersza poleceń"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Zapisywanie i odczytywanie argumentów wiersza poleceń jest niezbędną umiejętnością w programowaniu w Go. Pozwala to na bardziej interaktywne i spersonalizowane uruchomienie programu oraz uproszczenie procesu testowania i debugowania. W tym wpisie dowiecie się, dlaczego warto poznać tę funkcjonalność i jak ją zaimplementować w swoim kodzie.

## Jak to zrobić

Aby odczytywać argumenty wiersza poleceń w języku Go, używamy wbudowanej biblioteki `flag`. Najpierw importujemy ją w naszym kodzie przy użyciu polecenia `import "flag"`, a następnie tworzymy zmienne, do których będą przypisane odczytane argumenty. Możemy to zrobić za pomocą funkcji `flag.String()` dla zmiennych typu string, `flag.Int()` dla zmiennych typu int itp. Ważne jest, aby ustawić wartość domyślną, którą odczytamy, jeśli nie zostanie podany żaden argument. Następnie wywołujemy funkcję `flag.Parse()` po wszystkich deklaracjach zmiennych, aby odczytać i przypisać odpowiednie argumenty. Poniżej znajduje się przykładowy kod, który odczytuje dwa argumenty wiersza poleceń `name` i `age`.

```Go
import "flag"

func main() {
    name := flag.String("name", "Anonymous", "Person's name")
    age := flag.Int("age", 0, "Person's age")
    flag.Parse()

    fmt.Println("Hello,", *name, "! You are", *age, "years old.")
}
```

Aby uruchomić ten kod z argumentami `John` i `27`, należy wpisać w konsoli `go run main.go -name John -age 27`. Wynikiem powinno być wyświetlenie wiadomości `Hello, John! You are 27 years old.`.

## Głębsza analiza

W powyższym przykładzie wykorzystaliśmy funkcję `flag.String()` i `flag.Int()`, ale w zależności od typu argumentów, możemy użyć innych funkcji, takich jak `flag.Bool()` czy `flag.Float()` (lub ich odpowiedników z literą `Var` na końcu, co pozwala na przypisywanie wartości do już istniejących zmiennych). Jeśli chcemy, aby argument był wymagany, możemy użyć fukcji `flag.StringVar()` zamiast `flag.String()`, a także ustawić flagę `flag.Usage` z własną funkcją, aby wyświetlić użycie argumentów, jeśli zostanie podane niepoprawne wywołanie. Warto również pamiętać, że podejście opisane powyżej jest jednym ze sposobów odczytywania argumentów wiersza poleceń w języku Go, a istnieją inne biblioteki i rozwiązania, np. pakiet `doc`, które oferują inne funkcje i podejścia.

## Zobacz też

- [Dokumentacja pakietu `flag`](https://golang.org/pkg/flag/)
- [Dokumentacja pakietu `doc`](https://golang.org/pkg/doc/)
- [Przykładowy kod wykorzystujący bibliotekę `flag`](https://play.golang.org/p/KfJmlY6IlYr)