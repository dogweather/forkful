---
title:                "Generowanie losowych liczb"
html_title:           "Go: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie losowych liczb to ważna część programowania, gdyż pozwala na losowe działanie programów oraz na wprowadzenie elementu losowości do aplikacji. Programiści korzystają z generatorów losowych w celu testowania kodu, symulowania wyników i wiele innych.

## Jak to zrobić:

Wygenerowanie losowej liczby w języku Go jest bardzo proste - po prostu użyj polecenia "rand.Intn(n)", gdzie "n" to maksymalna wartość, jaką chcesz wygenerować. Przykładowy kod wyglądałby następująco:

```
Go package main

import (
	"fmt"
	"math/rand"
)

func main() {
	fmt.Println("Wylosowana liczba to:", rand.Intn(100))
}
```
W powyższym przykładzie, liczba losowa będzie z zakresu od 0 do 100.

## Głębsze zagadnienia:

Generowanie losowych liczb jest częścią programowania już od dawna, jednak istnieje wiele różnych sposobów ich generowania. W języku Go, używany jest algorytm "MT19937" do generowania liczb losowych. Alternatywą dla tego algorytmu jest na przykład "Xorshift" lub "Mersenne Twister".

## Zobacz również:

Aby dowiedzieć się więcej na temat generowania liczb losowych w języku Go, polecamy następujące źródła:
- Dokumentacja Go: https://golang.org/pkg/math/rand/
- Przewodnik po języku Go: https://tour.golang.org/basics/23
- Blog "The Go Programming Language": https://blog.golang.org/strings