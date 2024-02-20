---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:39.703723-07:00
description: "Generowanie losowych liczb w programowaniu polega na tworzeniu sekwencji\
  \ liczb, kt\xF3rych nie mo\u017Cna racjonalnie przewidzie\u0107 lepiej ni\u017C\
  \ przez przypadek.\u2026"
lastmod: 2024-02-19 22:04:54.030890
model: gpt-4-0125-preview
summary: "Generowanie losowych liczb w programowaniu polega na tworzeniu sekwencji\
  \ liczb, kt\xF3rych nie mo\u017Cna racjonalnie przewidzie\u0107 lepiej ni\u017C\
  \ przez przypadek.\u2026"
title: Generowanie liczb losowych
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie losowych liczb w programowaniu polega na tworzeniu sekwencji liczb, których nie można racjonalnie przewidzieć lepiej niż przez przypadek. Programiści robią to z wielu powodów, w tym w symulacjach, grach i zastosowaniach bezpieczeństwa, gdzie nieprzewidywalność jest kluczowa dla funkcjonalności lub tajemnicy.

## Jak to zrobić:

W Go, losowe liczby są generowane za pomocą pakietu `math/rand` dla pseudo-losowych liczb lub `crypto/rand` dla kryptograficznie bezpiecznych pseudo-losowych liczb. Zbadajmy oba.

### Używanie `math/rand` dla Pseudo-losowych Liczb

Najpierw zaimportuj pakiet `math/rand` i pakiet `time`, aby zasiać generator. Zasiew zapewnia, że za każdym razem dostajesz inną sekwencję liczb.

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Println("Losowa liczba:", rand.Intn(100)) // Generuje liczbę między 0 a 99
}
```

Przykładowy wynik: `Losowa liczba: 42`

### Używanie `crypto/rand` dla Kryptograficznie Bezpiecznych Pseudo-losowych Liczb

Dla aplikacji bardziej wrażliwych na bezpieczeństwo pakiet `crypto/rand` jest odpowiedni, ponieważ generuje losowe liczby trudne do przewidzenia, co czyni je odpowiednimi dla operacji kryptograficznych.

```go
package main

import (
	"crypto/rand"
	"fmt"
	"math/big"
)

func main() {
	n, _ := rand.Int(rand.Reader, big.NewInt(100))
	fmt.Println("Bezpieczna losowa liczba:", n)
}
```

Przykładowy wynik: `Bezpieczna losowa liczba: 81`

## Dogłębna analiza

Podstawowa różnica między pakietami `math/rand` a `crypto/rand` w Go wynika z ich źródła entropii i przeznaczenia. `math/rand` generuje pseudo-losowe liczby na podstawie początkowego ziarna; zatem sekwencja jest deterministyczna i może być przewidziana, jeśli ziarno jest znane. Jest to odpowiednie dla scenariuszy, gdzie kluczowe jest wysoka wydajność, a nie absolutna nieprzewidywalność, jak symulacje czy gry.

Z drugiej strony, `crypto/rand` czerpie losowość z podstawowego systemu operacyjnego, czyniąc go odpowiednim dla zastosowań kryptograficznych, gdzie nieprzewidywalność jest kluczowa. Jednak wiąże się to z kosztem wydajności i złożoności w obsłudze generowanych licz (jak radzenie sobie z typem `*big.Int` dla liczb całkowitych).

Historycznie, pojęcie generowania losowych liczb w komputerach zawsze balansowało na krawędzi prawdziwej "losowości", z wczesnymi systemami silnie zależnymi od deterministycznych algorytmów, które naśladowały losowość. Wraz z ewolucją komputerów, tak samo ewoluowały te algorytmy, włączając bardziej złożone źródła entropii ze swojego otoczenia.

Pomimo tych postępów, dążenie do doskonałej losowości w informatyce jest z natury paradoksalne, biorąc pod uwagę deterministyczny charakter samych komputerów. Dlatego, dla większości aplikacji, gdzie przewidywalność byłaby szkodliwa, kryptograficznie bezpieczne pseudo-losowe liczby z źródeł takich jak `crypto/rand` są lepszą alternatywą, pomimo ich obciążenia.

W istocie, podejście Go z dwoma odrębnymi pakietami do generowania losowych liczb elegancko rozwiązuje kompromis między wydajnością a bezpieczeństwem, pozwalając programistom wybierać w zależności od ich konkretnych potrzeb.
