---
title:                "Go: Odczytaj argumenty wiersza poleceń"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą Go i chcesz dowiedzieć się, jak czytać argumenty wiersza poleceń w swoim kodzie, ten artykuł jest dla Ciebie! Poznasz podstawowe informacje na temat tego, dlaczego warto umieć czytać argumenty wiersza poleceń oraz jak to zrobić w sposób prosty i efektywny.

## Jak to zrobić

Przygotowaliśmy dla Ciebie kod przykładowy, który pokaże Ci, jak wykorzystać pakiet "flag" w Go do czytania argumentów wiersza poleceń. Wystarczy dodać odpowiednie importy oraz kilka linijek kodu, a już będziesz miał możliwość czytania argumentów i wykorzystywania ich w swoim programie.

```Go
import "flag"

func main() {
    // deklaracja flagi "name" z domyślną wartością "world"
    name := flag.String("name", "world", "specify name to greet")
    // parsowanie argumentów wiersza poleceń
    flag.Parse()
    // wyświetlenie powitania z wykorzystaniem wartości flagi "name"
    fmt.Println("Hello", *name)
}
```

Przykładowe wywołanie programu z argumentem "John":
```
$ go run main.go -name=John
Hello John
```

## Deep Dive

Dalsza część artykułu jest dla osób, które chcą głębiej poznać temat czytania argumentów wiersza poleceń w Go. Omówimy w szczegółach pakiet "flag", jego funkcje oraz różne rodzaje flag, takie jak flagi tekstowe, liczbowe czy logiczne. Nauczysz się także, jak radzić sobie z błędnymi argumentami i jak korzystać z flag pozycyjnych.

## Zobacz też

- Oficjalna dokumentacja pakietu "flag": https://golang.org/pkg/flag/
- Przykładowy kod z wykorzystaniem pakietu "flag": https://github.com/golang/example/tree/master/flag
- Poradnik na temat czytania argumentów wiersza poleceń w Go: https://flaviocopes.com/go-command-line-arguments/