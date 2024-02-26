---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:40.153644-07:00
description: "Usuwanie znak\xF3w pasuj\u0105cych do okre\u015Blonego wzorca polega\
  \ na usuni\u0119ciu pewnych znak\xF3w lub sekwencji znak\xF3w ze \u0142a\u0144cuch\xF3\
  w tekstowych, na podstawie regu\u0142\u2026"
lastmod: '2024-02-25T18:49:33.256246-07:00'
model: gpt-4-0125-preview
summary: "Usuwanie znak\xF3w pasuj\u0105cych do okre\u015Blonego wzorca polega na\
  \ usuni\u0119ciu pewnych znak\xF3w lub sekwencji znak\xF3w ze \u0142a\u0144cuch\xF3\
  w tekstowych, na podstawie regu\u0142\u2026"
title: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca"
---

{{< edit_this_page >}}

## Co i dlaczego?

Usuwanie znaków pasujących do określonego wzorca polega na usunięciu pewnych znaków lub sekwencji znaków ze łańcuchów tekstowych, na podstawie reguł określonych przez wzorzec (zazwyczaj za pomocą wyrażeń regularnych). Programiści często muszą wykonywać to zadanie do czyszczenia danych, przetwarzania wstępnego dla analiz, formatowania wyjścia lub po prostu manipulowania łańcuchami znaków, aby spełnić wymagania aplikacji.

## Jak to zrobić:

W Go, usuwanie znaków pasujących do wzorca można efektywnie wykonać za pomocą pakietu `regexp`. Tutaj pokażemy, jak usunąć wszystkie cyfry, a następnie wszystkie znaki niealfanumeryczne z łańcucha znaków jako przykłady.

1. **Usuwanie wszystkich cyfr:**

```go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    text := "Go1 jest fajny, ale Go2 będzie fajniejszy! Teraz: 2023."
    
    // Skompiluj wyrażenie regularne dla cyfr
    re, err := regexp.Compile("[0-9]+")
    if err != nil {
        fmt.Println("Błąd kompilacji regex:", err)
        return
    }
    
    // Zastąp cyfry pustym łańcuchem znaków
    result := re.ReplaceAllString(text, "")
    
    fmt.Println(result) // Wyjście: Go jest fajny, ale Go będzie fajniejszy! Teraz: .
}
```

2. **Usuwanie wszystkich znaków niealfanumerycznych:**

```go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    text := "Go jest #1 @ języków programowania!"
    
    // Skompiluj wyrażenie regularne dla znaków niealfanumerycznych
    re, err := regexp.Compile("[^a-zA-Z0-9]+")
    if err != nil {
        fmt.Println("Błąd kompilacji regex:", err)
        return
    }
    
    // Zastąp znaki niealfanumeryczne pustym łańcuchem znaków
    result := re.ReplaceAllString(text, "")
    
    fmt.Println(result) // Wyjście: Gojest1językówprogramowania
}
```

## Dogłębna analiza

Pakiet `regexp` w Go zapewnia potężny interfejs do dopasowywania wzorców i manipulacji za pomocą wyrażeń regularnych. Jego implementacja pochodzi od RE2, biblioteki wyrażeń regularnych zaprojektowanej, aby gwarantować wykonanie w czasie liniowym, unikając możliwości problemów z "katastroficznym cofaniem się" obecnych w niektórych innych silnikach regex. To czyni regex w Go stosunkowo bezpiecznym i efektywnym dla szerokiej gamy aplikacji.

Chociaż pakiet `regexp` jest wszechstronnym rozwiązaniem do radzenia sobie ze wzorcami, warto zauważyć, że dla prostszych lub bardzo specyficznych manipulacji łańcuchami znaków, inne funkcje łańcucha znaków takie jak `strings.Replace()`, `strings.Trim()`, lub cięcie mogą oferować bardziej wydajne alternatywy. Wyrażenia regularne są potężnym narzędziem, ale ich względny koszt obliczeniowy oznacza, że dla operacji, które można określić bez ich użycia, eksplorowanie alternatyw biblioteki standardowej może czasami prowadzić do prostszego i bardziej wydajnego kodu.
