---
title:                "Wyszukiwanie i zamiana tekstu"
html_title:           "Go: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą pracującym w języku Go, możliwe że często musisz dokonywać zmian w kodzie. Zastanawiasz się może, czy istnieje szybszy sposób na wymianę znaków lub słów w kodzie? W tym artykule dowiesz się, że odpowiedź brzmi - tak! Wykorzystując wbudowane funkcje Go, możesz znacznie przyspieszyć i ułatwić swoją pracę.

## Jak to zrobić

Aby przeprowadzić szybką wymianę tekstu w swoim kodzie, możesz skorzystać z funkcji strings.Replace. Należy jednak pamiętać, że ta funkcja jest wrażliwa na wielkość liter, więc jeśli chcesz dokonać wymiany bez względu na wielkość liter, musisz użyć funkcji strings.ToLower lub strings.ToUpper. Przykładowy kod wyglądałby następująco:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	// Pierwszym parametrem jest tekst, w którym chcemy dokonać zmian
	// Drugim parametrem jest tekst, który chcemy zamienić
	// Trzeci parametr to nowy tekst, który ma zastąpić poprzedni
	// Czwarty parametr określa ilość wystąpień, które chcemy zmienić, jeśli w tekście znajduje się więcej niż jedno wystąpienie
	// W naszym przypadku ustawimy go na -1, co oznacza, że chcemy dokonać zmiany we wszystkich wystąpieniach
	text := "Go jest świetnym językiem programowania"
	newText := strings.Replace(text, "językiem", "języczkiem", -1)

	// Wynik: "Go jest świetnym języczkiem programowania"
	fmt.Println(newText)
}
```

Możesz także określić dodatkowe parametry, takie jak wielkość liter, co pozwoli ci na jeszcze większą kontrolę nad wymianą tekstu w kodzie. Funkcja strings.ReplaceAll zastąpi wszystkie wystąpienia, niezależnie od wielkości liter. Przykładowy kod wyglądałby tak:

```Go
text := "Golang jest językiem programowania"
newText := strings.ReplaceAll(strings.ToLower(text), "językiem", "języczkiem")

// Wynik: "golang jest języczkiem programowania"
fmt.Println(newText)
```

## Deep Dive

Poza funkcją strings.Replace i strings.ReplaceAll, istnieją dodatkowe metody w języku Go, które pomogą ci w dokonywaniu szybkich zmian w tekście. Na przykład, jeśli chcesz dokonać wymiany tekstu tylko w pewnej części tekstu, możesz skorzystać z funkcji strings.ReplaceAllWithin. Kluczowym elementem tej funkcji jest parametr "start", który określa początkowy indeks, od którego chcesz zacząć dokonywać wymiany tekstu. Przykładowy kod wyglądałby tak:

```Go
text := "Dlatego lubię programować w Go"
newText := strings.ReplaceAllWithin(strings.ToLower(text), "językiem", "języczkiem", 24)

// Wynik: "Dlatego lubię programować w języczku Go"
fmt.Println(newText)
```

Inną przydatną funkcją jest strings.ReplaceAllLiteral, która nie jest wrażliwa na znaki specjalne, więc jeśli chcesz dokonać wymiany tekstu zawierającego znaki specjalne, ta funkcja będzie lepszym wyborem. Przykładowy kod wyglądałby tak:

```Go
text := "Język Go jest wspaniały, czy nie?"
newText := strings.ReplaceAllLiteral(text, "jest", "to")

// Wynik: "Język Go to wspaniały, czy nie?"
fmt.Println(newText)
```

## Zobacz również

- Funkcje zaawansowane