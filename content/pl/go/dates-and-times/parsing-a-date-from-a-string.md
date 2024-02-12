---
title:                "Analiza składniowa daty z ciągu znaków"
aliases:
- /pl/go/parsing-a-date-from-a-string/
date:                  2024-02-03T18:05:20.223489-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analiza składniowa daty z ciągu znaków"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Parsowanie daty ze stringa w Go polega na konwersji daty przedstawionej w formie tekstu na bardziej użyteczny format (np. `time.Time`). Programiści wykonują to zadanie, aby dokładniej obsługiwać dane dotyczące daty i czasu w aplikacjach, szczególnie przy pracy z danymi wprowadzanymi przez użytkownika, API lub systemami przechowywania, gdzie daty często są reprezentowane jako stringi.

## Jak to zrobić:

Go zapewnia solidne wsparcie dla parsowania dat i czasów poprzez pakiet `time`. Kluczem jest zrozumienie formatu referencyjnego daty w Go: `Mon Jan 2 15:04:05 MST 2006`, który używasz, aby powiedzieć Go, jak interpretować przychodzący string. Oto szybki przykład, który pomoże Ci zacząć:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Przykładowy string z datą
	dateStr := "2023-04-12 14:45:00"
	
	// Definicja układu/formatu przychodzącego stringa z datą
	// Ten układ informuje Go, aby oczekiwać roku, następnie miesiąca, 
	// dnia, godziny, minuty, a na końcu sekundy
	layout := "2006-01-02 15:04:05"
	
	// Parsowanie stringa z datą zgodnie z układem
	parsedDate, err := time.Parse(layout, dateStr)
	if err != nil {
		fmt.Println("Błąd przy parsowaniu daty:", err)
		return
	}
	
	// Wypisanie sparsowanej daty
	fmt.Println("Sparsowana Data:", parsedDate)
}
```

Kiedy uruchomisz ten kod, otrzymasz:

```
Sparsowana Data: 2023-04-12 14:45:00 +0000 UTC
```

Zauważ, jak string `layout` używa wartości daty referencyjnej do określenia formatu przychodzącego stringa. Dostosuj `layout`, aby pasował do formatu Twoich wejściowych dat.

## Zejście w głąb

Projektowanie parsowania dat i czasu w Go jest unikalne, wykorzystuje specyficzną datę referencyjną (`Mon Jan 2 15:04:05 MST 2006`). To podejście, zamiast używania bardziej konwencjonalnych specyfikatorów formatu (jak `YYYY` dla roku), zostało wybrane ze względu na czytelność i łatwość użycia, opierając się na formacie bardziej opartym na przykładach.

Chociaż początkowo może to wydawać się niezwykłe dla programistów przyzwyczajonych do innych języków, wielu znajduje to bardziej intuicyjne po krótkim okresie dostosowania. Dla aplikacji wymagających bardziej złożonej manipulacji datami lub formatów nieobsługiwanych bezpośrednio przez pakiet `time` Go, biblioteki stron trzecich, takie jak `github.com/jinzhu/now`, mogą oferować dodatkową funkcjonalność. Jednak dla większości standardowych aplikacji, wbudowane możliwości Go są solidne, wydajne i idiomatyczne, odzwierciedlając filozofię Go, która opiera się na prostocie i klarowności.
