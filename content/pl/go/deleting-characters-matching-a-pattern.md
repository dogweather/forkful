---
title:                "Usuwanie znaków pasujących do wzorca."
html_title:           "Go: Usuwanie znaków pasujących do wzorca."
simple_title:         "Usuwanie znaków pasujących do wzorca."
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Pewnie zastanawiasz się dlaczego ktokolwiek miałby chcieć usuwać znaki zgodne z pewnym wzorem. Jednym z powodów może być potrzeba zmiany lub oczyszczenia danych, aby ułatwić analizę lub przetwarzanie. Może też po prostu chcesz uporządkować dane i pozbyć się zbędnych znaków.

## Jak to zrobić

Go oferuje wiele możliwości do usuwania znaków zgodnych z wzorem. Najprostszym sposobem jest użycie funkcji `ReplaceAllString` z pakietu `regexp`. Zobaczmy jak to wygląda w kodzie:

```Go
// Importowanie odpowiadających pakietów
import (
    "fmt"
    "regexp"
)

func main() {
    // Definicja stringa z którego chcemy usunąć znaki
    input := "K%o(d)e P@o(l)i'sh Str&in%g"
    
    // Utworzenie wyrażenia regularnego pasującego do znaków specjalnych
    pattern := regexp.MustCompile("[%$&@()]")
    
    // Użycie funkcji ReplaceAllString do usunięcia pasujących znaków
    output := pattern.ReplaceAllString(input, "")
    
    // Wyświetlenie efektu na ekranie
    fmt.Println(output)
}
```

Wyjście z tego programu będzie wyglądać następująco:

```
Kode Polish String
```

Możesz także użyć funkcji `ReplaceAll` z pakietu `strings` jeśli nie potrzebujesz użycia wyrażenia regularnego. Oto przykład:

```Go
// Improtowanie odpowiadających pakietów
import (
    "fmt"
    "strings"
)

func main() {
    // Definicja stringa z którego chcemy usunąć znaki
    input := "Hello 123 World 456"
    
    // Użycie funkcji ReplaceAll do usunięcia cyfr
    output := strings.ReplaceAll(input, "123", "")
    output = strings.ReplaceAll(output, "456", "")
    
    // Wyświetlenie efektu na ekranie
    fmt.Println(output)
}
```

Wyjście z tego programu będzie wyglądać następująco:

```
Hello World
```

## Deep Dive

Jeśli chcesz dowiedzieć się więcej o usuwaniu znaków zgodnych z wzorem w Go, warto zapoznać się z dokumentacją pakietu `regexp`. Możesz przeczytać o różnych funkcjach i sposobach wykorzystania wyrażeń regularnych w Go. Istnieje także wiele innych pakietów z funkcjami do operacji na stringach, z których możesz skorzystać w zależności od swoich potrzeb.

## Zobacz też

- https://golang.org/pkg/regexp/ - Dokumentacja pakietu `regexp`
- https://golang.org/pkg/strings/ - Dokumentacja pakietu `strings`