---
title:                "Praca z liczbami zespolonymi"
date:                  2024-01-26T04:41:13.953959-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z liczbami zespolonymi"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Liczby zespolone, złożone z części rzeczywistej i urojonej (jak 5 + 7i), są kluczowe w dziedzinach takich jak inżynieria, fizyka i przetwarzanie sygnałów. Programiści pracują z nimi, aby rozwiązywać problemy w tych dziedzinach, które byłyby trudne do rozwiązania tylko za pomocą liczb rzeczywistych.

## Jak to zrobić:
Go oferuje wbudowane wsparcie dla liczb zespolonych. Oto krótki przegląd:

```go
package main

import (
	"fmt"
	"math/cmplx"
)

func main() {
	// Tworzenie liczb zespolonych
	a := complex(2, 3)
	b := 4 + 5i

	// Podstawowe operacje
	fmt.Println("Dodawanie:", a+b)
	fmt.Println("Odejmowanie:", a-b)
	fmt.Println("Mnożenie:", a*b)
	fmt.Println("Dzielenie:", a/b)

	// Właściwości liczby zespolonej
	fmt.Println("Część rzeczywista:", real(b))
	fmt.Println("Część urojona:", imag(b))
	fmt.Println("Sprzężenie:", cmplx.Conj(b))
	fmt.Println("Moduł:", cmplx.Abs(b))
	fmt.Println("Kąt fazowy (radiany):", cmplx.Phase(b))
}

```

Przykładowe wyjście:

```
Dodawanie: (6+8i)
Odejmowanie: (-2-2i)
Mnożenie: (-7+22i)
Dzielenie: (0.5609756097560976+0.0487804878048781i)
Część rzeczywista: 4
Część urojona: 5
Sprzężenie: (4-5i)
Moduł: 6.4031242374328485
Kąt fazowy (radiany): 0.8960553845713439
```

## Wnikliwe spojrzenie
Dawno temu, liczby zespolone były traktowane z podejrzliwością – niektórzy uważali je za bezużyteczne! Z czasem stało się jasne, że mają moc w opisywaniu zjawisk fizycznych. Są one fundamentalne w fizyce kwantowej, teorii sterowania i inżynierii elektrycznej, aby wymienić tylko kilka obszarów.

W Go, liczby zespolone są reprezentowane za pomocą typu danych zwany `complex128` (64 bity na część rzeczywistą i urojoną) lub `complex64` (32 bity na każdą). W podszewce, są to po prostu dwie `float64` lub `float32` połączone razem. Standardowa biblioteka Go, `math/cmplx`, oferuje funkcje do operacji matematycznych na liczbach zespolonych. Oszczędza to przed ciężką matematyką i pozwala skupić się na rozwiązywaniu problemów.

Alternatywy dla wbudowanego wsparcia Go obejmują korzystanie z zewnętrznych bibliotek lub tworzenie własnego obsługiwania liczb zespolonych. Ale rzadko są one potrzebne, ponieważ rodzime wsparcie Go jest wydajne i dobrze zintegrowane z językiem.

## Zobacz także
Sprawdź te linki, aby dowiedzieć się więcej o możliwościach Go w zakresie liczb zespolonych:
- Oficjalna dokumentacja Go: https://golang.org/pkg/math/cmplx/
- Dogłębne odświeżenie matematyki na temat liczb zespolonych: https://www.mathsisfun.com/numbers/complex-numbers.html
- Praktyczne zastosowania liczb zespolonych w inżynierii: https://ieeexplore.ieee.org/document/528dunno
