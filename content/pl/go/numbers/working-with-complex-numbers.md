---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:26.133187-07:00
description: "Jak to zrobi\u0107: W Go, liczby zespolone s\u0105 obs\u0142ugiwane\
  \ za pomoc\u0105 wbudowanych funkcji `complex`, `real` i `imag`, wraz z typami `complex64`\
  \ i `complex128`\u2026"
lastmod: '2024-03-13T22:44:34.842788-06:00'
model: gpt-4-0125-preview
summary: "W Go, liczby zespolone s\u0105 obs\u0142ugiwane za pomoc\u0105 wbudowanych\
  \ funkcji `complex`, `real` i `imag`, wraz z typami `complex64` i `complex128` (reprezentuj\u0105\
  ce odpowiednio 64-bitowe i 128-bitowe liczby zespolone)."
title: Praca z liczbami zespolonymi
weight: 14
---

## Jak to zrobić:
W Go, liczby zespolone są obsługiwane za pomocą wbudowanych funkcji `complex`, `real` i `imag`, wraz z typami `complex64` i `complex128` (reprezentujące odpowiednio 64-bitowe i 128-bitowe liczby zespolone). Oto krótki przewodnik:

```go
package main

import (
	"fmt"
)

func main() {
	// Tworzenie liczb zespolonych
	a := complex(2, 3) // 2+3i
	b := complex(1, -1) // 1-1i

	// Operacje arytmetyczne
	c := a + b
	fmt.Println("Dodawanie:", c) // Wynik: Dodawanie: (3+2i)

	d := a * b
	fmt.Println("Mnożenie:", d) // Wynik: Mnożenie: (5+1i)

	// Dostęp do części rzeczywistej i urojonej
	czescRzeczywista := real(a)
	czescUrojona := imag(a)
	fmt.Printf("Część rzeczywista: %.1f, Część urojona: %.1f\n", czescRzeczywista, czescUrojona) // Wynik: Część rzeczywista: 2.0, Część urojona: 3.0

	// Sprzężenie zespolone i moduł mogą być obliczone
	sprzezenie := complex(real(a), -imag(a)) // Ręcznie
	fmt.Println("Sprzężenie liczby a:", sprzezenie) // Wynik: Sprzężenie liczby a: (2-3i)
}
```

Ten przykład pokrywa podstawy, ale jest znacznie więcej, co można zrobić z liczbami zespolonymi, w tym wykorzystywanie pakietu `math/cmplx` dla bardziej zaawansowanych operacji, tak jak znajdowanie modułu, fazy i wiele więcej.

## W głębi tematu
Koncepcja liczb zespolonych sięga XVI wieku, ale szerokie rozpoznanie i rygorystyczna formalizacja nastąpiła dopiero w XIX wieku. W programowaniu komputerowym liczby zespolone są stałym elementem skomplikowanych obliczeń arytmetycznych w naukowych i inżynieryjnych kalkulacjach od wczesnych lat. Podejście Go do liczb zespolonych, poprzez uczynienie ich obywatelem pierwszej klasy z wbudowanym wsparciem i obszernym wsparciem standardowej biblioteki poprzez pakiet `math/cmplx`, wyróżnia się wśród języków programowania. Ta decyzja projektowa odzwierciedla nacisk Go na prostotę i wydajność.

Niemniej jednak warto zauważyć, że praca z liczbami zespolonymi w Go, choć potężna, może nie zawsze być najlepszym podejściem dla wszystkich aplikacji, szczególnie tych wymagających matematyki symbolicznej lub arytmetyki wysokiej precyzji. Języki i środowiska specjalizujące się w obliczeniach naukowych, takie jak Python z bibliotekami takimi jak NumPy i SciPy, lub oprogramowanie takie jak MATLAB, mogą oferować większą elastyczność i szerszy zakres funkcjonalności dla konkretnych zastosowań.

Mimo to, dla programowania systemowego i kontekstów, w których integracja obliczeń liczbowych zespolonych z większą, wrażliwą na wydajność aplikacją jest kluczowa, natywne wsparcie Go dla liczb zespolonych zapewnia wyjątkowo wydajną opcję.
