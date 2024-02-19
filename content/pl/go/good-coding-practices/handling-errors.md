---
aliases:
- /pl/go/handling-errors/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:20.765570-07:00
description: "Obs\u0142uga b\u0142\u0119d\xF3w w Go polega na rozpoznawaniu i reagowaniu\
  \ na sytuacje wyj\u0105tkowe w programie. Programi\u015Bci zajmuj\u0105 si\u0119\
  \ obs\u0142ug\u0105 b\u0142\u0119d\xF3w, aby zapewni\u0107, \u017Ce ich\u2026"
lastmod: 2024-02-18 23:08:49.106581
model: gpt-4-0125-preview
summary: "Obs\u0142uga b\u0142\u0119d\xF3w w Go polega na rozpoznawaniu i reagowaniu\
  \ na sytuacje wyj\u0105tkowe w programie. Programi\u015Bci zajmuj\u0105 si\u0119\
  \ obs\u0142ug\u0105 b\u0142\u0119d\xF3w, aby zapewni\u0107, \u017Ce ich\u2026"
title: "Obs\u0142uga b\u0142\u0119d\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?

Obsługa błędów w Go polega na rozpoznawaniu i reagowaniu na sytuacje wyjątkowe w programie. Programiści zajmują się obsługą błędów, aby zapewnić, że ich aplikacje mogą elegancko radzić sobie z nieoczekiwanymi sytuacjami, co prowadzi do tworzenia bardziej niezawodnego i solidnego oprogramowania.

## Jak to zrobić:

W Go obsługa błędów jest zarządzana w sposób jawny za pomocą typu `error`. Funkcje, które mogą zakończyć się niepowodzeniem, zwracają błąd jako swoją ostatnią wartość zwracaną. Sprawdzenie, czy ta wartość błędu jest równa `nil`, powie ci, czy wystąpił błąd.

```go
package main

import (
    "errors"
    "fmt"
)

func Compute(value int) (int, error) {
    if value > 100 {
        return 0, errors.New("wartość musi być 100 lub mniejsza")
    }
    return value * 2, nil
}

func main() {
    result, err := Compute(150)
    if err != nil {
        fmt.Println("Błąd:", err)
    } else {
        fmt.Println("Wynik:", result)
    }
    
    // Elegancka obsługa błędu
    anotherResult, anotherErr := Compute(50)
    if anotherErr != nil {
        fmt.Println("Błąd:", anotherErr)
    } else {
        fmt.Println("Wynik:", anotherResult)
    }
}
```

Przykładowe wyjście dla powyższego kodu:
```
Błąd: wartość musi być 100 lub mniejsza
Wynik: 100
```

W tym przykładzie funkcja `Compute` zwraca albo obliczoną wartość, albo błąd. Wywołujący radzi sobie z błędem, sprawdzając, czy `err` nie jest równy `nil`.

## Dogłębna analiza

Podejście Go do obsługi błędów jest świadomie proste i bezpieczne typowo, wymagające jawnych sprawdzeń błędów. Ta koncepcja kontrastuje z obsługą błędów opartą na wyjątkach, widoczną w językach takich jak Java i Python, gdzie błędy są propagowane w górę stosu wywołań, chyba że zostaną przechwycone przez obsługę wyjątków. Zespół Go argumentuje, że jawna obsługa błędów prowadzi do bardziej przejrzystego i niezawodnego kodu, ponieważ zmusza programistów do natychmiastowego zajęcia się błędami w miejscu, w którym występują.

Jednak niektóre krytyki wskazują, że ten wzorzec może prowadzić do rozwlekłego kodu, zwłaszcza w skomplikowanych funkcjach z wieloma operacjami podatnymi na błędy. W odpowiedzi na to nowsze wersje Go wprowadziły bardziej zaawansowane funkcje obsługi błędów, takie jak zawijanie błędów, ułatwiając dostarczanie kontekstu błędu bez utraty oryginalnych informacji o błędzie. Społeczność widziała również propozycje nowych mechanizmów obsługi błędów, takich jak sprawdzanie/obsługa, chociaż te wciąż pozostają przedmiotem dyskusji według mojej ostatniej aktualizacji.

Filozofia obsługi błędów w Go kładzie nacisk na rozumienie i planowanie błędów jako części normalnego przepływu programu. Podejście to zachęca do tworzenia bardziej odpornego i przewidywalnego oprogramowania, choć może to wiązać się ze wzrostem ilości kodu szablonowego. Istnieją alternatywne wzorce i biblioteki mające na celu uproszczenie obsługi błędów w szczególnie skomplikowanych przypadkach, jednak wbudowany typ `error` pozostaje fundamentem obsługi błędów w języku.
