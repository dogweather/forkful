---
title:                "Generowanie liczb losowych"
date:                  2024-01-27T20:33:37.172993-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generowanie liczb losowych"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie liczb losowych w Go polega na wykorzystaniu pakietu `math/rand` do produkcji liczb pseudolosowych dla różnych zastosowań, takich jak symulowanie eksperymentów, generowanie danych testowych lub dodawanie nieprzewidywalności do gier. Programiści korzystają z tej funkcji, aby tworzyć dynamiczne i mniej przewidywalne zachowania oprogramowania.

## Jak to zrobić:

Aby rozpocząć generowanie liczb losowych w Go, musisz zaimportować pakiet `math/rand` oraz pakiet `time`, aby zasilić generator liczb losowych w celu zwiększenia nieprzewidywalności. Oto podstawowy przykład:

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Zasilenie generatora
	rand.Seed(time.Now().UnixNano())
	
	// Generowanie losowej liczby całkowitej z zakresu 0 do 99
	randomInt := rand.Intn(100)
	fmt.Println("Losowa liczba całkowita:", randomInt)
	
	// Generowanie losowej liczby zmiennoprzecinkowej z zakresu 0.0 do 1.0
	randomFloat := rand.Float64()
	fmt.Println("Losowa liczba zmiennoprzecinkowa:", randomFloat)
}
```

Przykładowy wynik może wyglądać tak:

```
Losowa liczba całkowita: 42
Losowa liczba zmiennoprzecinkowa: 0.7304601899194229
```

Pamiętaj, że każde wykonanie daje różne liczby ze względu na zasianie aktualnym czasem.

## W głąb materii

Pakiet `math/rand` w Go implementuje generatory liczb pseudolosowych (PRNGs) dla różnych rozkładów. Pomimo dużiej skuteczności w wielu aplikacjach, ważne jest, aby zauważyć, że liczby generowane przez `math/rand` nie nadają się do celów kryptograficznych ze względu na ich deterministyczną naturę. Dla potrzeb kryptograficznych odpowiednim wyborem jest pakiet `crypto/rand`, zapewniający bezpieczny generator liczb losowych.

Implementacja `math/rand` opiera się na algorytmie subtraktywnego generatora liczb losowych, który jest wydajny i ma stosunkowo długi okres przed powtarzaniem sekwencji. Jednak dla aplikacji wymagających prawdziwie losowych sekwencji, takich jak operacje kryptograficzne, zalecane są generatory liczb losowych sprzętowe (RNGs) lub pakiet `crypto/rand`, który wchodzi w interakcję z systemowo-szczegółowymi bezpiecznymi źródłami losowości.

`math/rand` pozwala na zasianie, aby wprowadzić zmienność, ale ten sam nasion będzie zawsze generował tę samą sekwencję liczb, co podkreśla deterministyczną naturę jego losowości. Sprawia to, że jest odpowiedni dla symulacji lub gier, gdzie reprodukowalność może być pożądana do celów debugowania lub testowania.
