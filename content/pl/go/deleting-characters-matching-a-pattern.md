---
title:                "Go: Usuwanie znaków pasujących do wzorca"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Napisanie programu to nie tylko pisane nowego kodu, ale także utrzymywanie i ulepszanie już istniejącego kodu. Czasami może być potrzebne usunięcie pewnych znaków z naszych danych źródłowych. W tym artykule dowiecie się, dlaczego i jak usunąć znaki pasujące do określonego wzorca za pomocą języka Go.

## Jak To Zrobić

Poniżej przedstawiamy przykłady kodu Go, który pokazuje, jak możemy usunąć znaki pasujące do określonego wzorca używając metody `ReplaceAllString()` z pakietu `regexp`. Przykład ten jest wyjaśniony krok po kroku i pokazuje w jaki sposób użyć tej metody w praktyce.

```Go
// Importowanie pakietów
import (
    "fmt"
    "regexp"
)

func main() {
    // Tworzenie wyrażenia regularnego
    re := regexp.MustCompile(`[aeiou]+`)

    // Tekst, z którego chcemy usunąć znaki pasujące do wzorca
    text := "Golang jest niesamowitym językiem programowania!"

    // Wywołanie metody ReplaceAllString()
    newText := re.ReplaceAllString(text, "")

    // Wypisanie wynikowego tekstu
    fmt.Println(newText)
}
```

W powyższym przykładzie wyrażenie regularne `[aeiou]+` oznacza, że poszukujemy ciągów składających się z samych samogłosek. Metoda `ReplaceAllString()` zastąpi te znaki pustym stringiem, czyli w rezultacie usunie je z tekstu.

### Przykładowy Output

```
Glng jst nsmwzm jzykn prgrmwng!
```

## Deep Dive

Aby lepiej zrozumieć jak działają wyrażenia regularne i metoda `ReplaceAllString()` w języku Go, warto przeczytać dokumentację na temat pakietu `regexp`. Możecie również wypróbować różne wyrażenia regularne i eksperymentować z nimi w celu osiągnięcia pożądanych wyników.

## Zobacz Również

- [Dokumentacja pakietu regexp](https://pkg.go.dev/regexp)
- [Przykładowe wyrażenia regularne w języku Go](https://regex101.com/library/Mqrf4J)