---
title:                "Znajdowanie długości łańcucha"
aliases: - /pl/go/finding-the-length-of-a-string.md
date:                  2024-02-03T17:57:04.445716-07:00
model:                 gpt-4-0125-preview
simple_title:         "Znajdowanie długości łańcucha"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/finding-the-length-of-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Znalezienie długości ciągu znaków w Go polega na określeniu liczby znaków, które zawiera. Programiści regularnie wykonują tę operację, aby efektywnie manipulować ciągami znaków, czy to dla celów weryfikacji, ekstrakcji podciągów, czy po prostu do egzekwowania ograniczeń w danych wejściowych użytkownika.

## Jak to zrobić:
W Go, ciągi znaków traktowane są jako niezmienne sekwencje bajtów. Długość ciągu znaków można znaleźć za pomocą wbudowanej funkcji `len()`, która zwraca liczbę bajtów, a niekoniecznie liczbę znaków. Oto jak jej użyć:

```go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	// Używanie len() do znalezienia długości w bajtach
	str := "Hello, 世界"
	byteLength := len(str)
	fmt.Println("Długość w bajtach:", byteLength) // Wyjście: Długość w bajtach: 13

	// Aby dokładnie uzyskać liczbę znaków lub runów w ciągu znaków
	runeLength := utf8.RuneCountInString(str)
	fmt.Println("Długość Runów:", runeLength) // Wyjście: Długość Runów: 9
}
```
Pierwsza metoda przy użyciu `len()` może nie zawsze dawać oczekiwany wynik, ponieważ liczy bajty. Dla ciągów zawierających znaki inne niż ASCII (jak "世界"), zamiast tego należy użyć `RuneCountInString` z pakietu `unicode/utf8`, aby dokładnie zliczyć punkty kodowe Unicode.

## Pogłębiona analiza
Przed Go 1 nie było ścisłego rozgraniczenia dla obsługi ciągów znaków jako sekwencje bajtów w porównaniu z sekwencjami znaków. Po Go 1, przyjęcie UTF-8 jako standardowego schematu kodowania dla ciągów znaków wymagało jaśniejszych podejść. Funkcja `len()` doskonale nadaje się do ciągów ASCII, gdzie znaki są reprezentowane w pojedynczym bajcie. Jednak, gdy aplikacje Go stały się bardziej globalne, a potrzeba wsparcia mnóstwa języków i zestawów znaków wzrosła, proste podejście `len()` pokazało ograniczenia.

Wprowadzenie i użycie `utf8.RuneCountInString()` odpowiada na te ograniczenia, zapewniając sposób na zliczanie rzeczywistych znaków Unicode (runów w terminologii Go). Ta metoda zapewnia, że obliczenie długości jest niezależne od specyfiki kodowania UTF-8, gdzie znaki mogą zajmować wiele bajtów.

Alternatywne podejście do przeglądania i manipulowania ciągami znaków, bardziej zgodne z etosem współbieżności i efektywności Go, może polegać na traktowaniu ciągów znaków jako wycinków runów. Jednak ta metoda wymaga kroku konwersji i nie rozwiązuje natychmiast wszystkich subtelności Unicode (np. znaków łączących).

Podsumowując, chociaż `len()` nadaje się do określania długości w bajtach i jest wydajne dla tekstu ASCII, `utf8.RuneCountInString()` jest bardziej niezawodnym wyborem dla aplikacji kompatybilnej globalnie. Mimo to, zachęca się programistów do zrozumienia kompromisów w zakresie wydajności i użycia pamięci, które te wybory niosą.
