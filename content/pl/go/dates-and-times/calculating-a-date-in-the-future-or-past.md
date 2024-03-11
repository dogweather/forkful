---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:52:56.743439-07:00
description: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci w\
  \ Go polega na manipulowaniu warto\u015Bciami daty i czasu, aby okre\u015Bli\u0107\
  \ konkretny punkt wzgl\u0119dem podanej daty.\u2026"
lastmod: '2024-03-11T00:14:08.039261-06:00'
model: gpt-4-0125-preview
summary: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci w Go polega\
  \ na manipulowaniu warto\u015Bciami daty i czasu, aby okre\u015Bli\u0107 konkretny\
  \ punkt wzgl\u0119dem podanej daty.\u2026"
title: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci"
---

{{< edit_this_page >}}

## Co i dlaczego?

Obliczanie daty w przyszłości lub przeszłości w Go polega na manipulowaniu wartościami daty i czasu, aby określić konkretny punkt względem podanej daty. Programiści powszechnie wykonują to zadanie dla aplikacji wymagających harmonogramowania, terminów, przypomnień lub dowolnej funkcjonalności, gdzie postęp lub regresja czasu jest niezbędna.

## Jak to zrobić:

Go udostępnia pakiet `time` do obsługi operacji na datach i czasie, oferując proste mechanizmy dodawania lub odejmowania czasu. Oto spojrzenie na wykorzystanie pakietu `time` do obliczania dat przyszłych lub przeszłych:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Obecna data i czas
	now := time.Now()
	fmt.Println("Obecna data i czas: ", now)

	// Obliczanie daty 10 dni w przyszłości
	futureDate := now.AddDate(0, 0, 10)
	fmt.Println("Data 10 dni w przyszłości: ", futureDate)
	
	// Obliczanie daty 30 dni w przeszłości
	pastDate := now.AddDate(0, 0, -30)
	fmt.Println("Data 30 dni w przeszłości: ", pastDate)
	
	// Dodanie 5 godzin i 30 minut do obecnej daty i czasu
	futureTime := now.Add(5*time.Hour + 30*time.Minute)
	fmt.Println("Przyszły czas (5 godzin i 30 minut później): ", futureTime)
}
```

Przykład wyjścia:
```
Obecna data i czas:  2023-04-01 15:04:05.123456789 +0000 UTC
Data 10 dni w przyszłości:  2023-04-11 15:04:05.123456789 +0000 UTC
Data 30 dni w przeszłości:  2023-03-02 15:04:05.123456789 +0000 UTC
Przyszły czas (5 godzin i 30 minut później):  2023-04-01 20:34:05.123456789 +0000 UTC
```
Zauważ, jak metoda `AddDate` jest używana do manipulacji datą przez lata, miesiące i dni, podczas gdy metoda `Add` jest używana do bardziej precyzyjnych delt czasu, takich jak godziny, minuty i sekundy.

## Głębsze zanurzenie

Pakiet `time` języka programowania Go ułatwia manipulację czasem z silnym bezpieczeństwem typu i jasną składnią, cechy, za które Go jest bardzo cenione. Jego implementacja opiera się na funkcjonalnościach manipulacji czasem dostarczanych przez leżący u podstaw system operacyjny, zapewniając efektywność i dokładność. Historycznie rzecz biorąc, obsługa dat i czasu w programowaniu była pełna złożoności z powodu różnic w strefach czasowych, latach przestępnych i zmianach czasu letniego. Pakiet `time` w Go abstrahuje dużą część tej złożoności, oferując programistom solidne narzędzia do manipulacji czasem.

Chociaż rodzimy pakiet `time` Go obejmuje szeroki zakres potrzeb manipulacji czasem, alternatywne biblioteki, takie jak `github.com/jinzhu/now`, oferują dodatkowe udogodnienia i funkcjonalności dla bardziej specyficznych przypadków użycia. Te alternatywy mogą być szczególnie użyteczne dla bardziej złożonych potrzeb manipulacji datą i czasem, nieobsługiwanych bezpośrednio przez rodzimy pakiet `time`.

Jednakże, dla większości aplikacji, wbudowane w Go możliwości manipulacji czasem zapewniają solidną podstawę. Balansują one między wydajnością a łatwością użycia, zapewniając, że programiści mogą efektywnie radzić sobie z większością powszechnych zadań związanych z czasem, nie sięgając po pakiety stron trzecich.
