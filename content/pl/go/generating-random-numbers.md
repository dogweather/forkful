---
title:                "Go: Generowanie losowych liczb"
programming_language: "Go"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie liczb losowych pełni istotną rolę w wielu dziedzinach programowania. Może być wykorzystywane do testowania algorytmów, tworzenia gier, lub nawet do wylosowania zwycięskich numerów w loterii. W tym artykule dowiesz się jak wygenerować losowe liczby w języku Go i jakie techniki mogą zostać wykorzystane.

## Jak to zrobić

Aby wygenerować losowe liczby w Go, musimy najpierw zaimportować pakiet "math/rand". Następnie możemy wywołać funkcję "rand.Intn()", która zwraca losową liczbę całkowitą o podanym zakresie. Oto przykładowy kod:

```Go
import "math/rand"

// Wygenerowanie losowej liczby od 0 do 100
rand.Intn(101)
```

Jeśli chcemy, aby liczby były generowane w oparciu o jakiś seed, co pozwala na powtarzalność wyników, możemy wykorzystać funkcję "rand.NewSource()" wraz z funkcją "rand.New()". Oto przykładowy kod:

```Go
// Ustawienie seed na 42
source := rand.NewSource(42)
rand := rand.New(source)

// Wygenerowanie losowej liczby od 0 do 100
rand.Intn(101)
```

Możemy również wykorzystać inne funkcje z pakietu "math/rand" do generowania różnych typów liczb, takich jak liczby zmiennoprzecinkowe lub liczby losowe z przedziału od innej liczby. Zachęcam do eksperymentowania z różnymi funkcjami i sposobami generacji liczb losowych.

## Głębsze zagadnienia

Generowanie liczb losowych może być trudne ze względu na to, że komputery nie są w stanie wygenerować naprawdę losowych liczb. Zwykle wykorzystują one określone algorytmy i seed, aby generować liczby, które wydają się losowe dla nas, ale tak naprawdę są one deterministyczne. Dlatego też, jeśli potrzebujesz naprawdę losowych liczb losowych, powinieneś rozważyć wykorzystanie zewnętrznych źródeł jak np. losujących karty lub kostki.

W języku Go możemy również wykorzystać pakiet "crypto/rand", który jest bardziej bezpieczny i wykorzystuje losowość systemową. Oto przykładowy kod:

```Go
import "crypto/rand"

var bytes []byte = make([]byte, 5)

// Wygenerowanie losowych bajtów
rand.Read(bytes)
```

## Zobacz też

- [Dokumentacja pakietu math/rand w języku Go](https://golang.org/pkg/math/rand/)
- [Dokumentacja pakietu crypto/rand w języku Go](https://golang.org/pkg/crypto/rand/)
- [Artykuł na temat generowania liczb losowych w języku Go](https://blog.gopheracademy.com/advent-2017/pitfalls-of-random-number-generation/)