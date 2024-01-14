---
title:    "Go: Wyszukiwanie i zamienianie tekstu"
keywords: ["Go"]
---

{{< edit_this_page >}}

# Dlaczego warto używać funkcji wyszukiwania i zastępowania tekstu w języku Go?

Jeśli tworzysz aplikacje w języku Go, prawdopodobnie napotkasz sytuacje, w których będziesz musiał zmienić lub zastąpić pewien fragment tekstu w swoim kodzie. Funkcje wyszukiwania i zastępowania tekstu są bardzo przydatne i ułatwiają znalezienie i zmianę konkretnego wyrażenia w całym kodzie.

## Jak to zrobić?

Poniżej znajdziesz przykładowy kod w języku Go, który ilustruje jak można użyć funkcji wyszukiwania i zastępowania tekstu.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	// Tworzymy przykładowy tekst
	text := "Dzień Dobry! Witaj w moim blogu o języku Go. Jestem bardzo podekscytowany dzieleniem się z Tobą moją wiedzą."

	// Wyszukujemy wyrażenie "Go" i zastępujemy je frazą "Golang"
	replacedText := strings.Replace(text, "Go", "Golang", -1)

	// Wypisujemy nowy tekst z zastąpionym wyrażeniem
	fmt.Println(replacedText)
}

```

W powyższym kodzie funkcja Replace() z pakietu strings została użyta do znalezienia i zastąpienia wyrażenia "Go" frazą "Golang". Dzięki temu, nasz przykładowy tekst został zmieniony i wypisze się "Dzień Dobry! Witaj w moim blogu o języku Golang. Jestem bardzo podekscytowany dzieleniem się z Tobą moją wiedzą.".

Funkcja Replace() posiada trzeci argument "n", który definiuje ile razy powinna zostać wykonana zamiana wyrażenia. W powyższym przykładzie użyliśmy wartości "-1", co oznacza, że wszystkie wystąpienia wyrażenia zostaną zastąpione.

## Głębsza analiza

Funkcja Replace() jest częścią pakietu strings, który dostarcza zestaw funkcji wykorzystywanych do pracy ze stringami. Funkcja ta może być używana nie tylko do zastępowania wyrażeń, ale również do wielu innych operacji na tekstach, takich jak wyodrębnianie fragmentów tekstu czy zmiana wielkości liter. Warto zapoznać się z dokumentacją tego pakietu, aby poznać wszystkie dostępne funkcje.

## Zobacz również 
- Dokumentacja pakietu strings w języku Go: https://golang.org/pkg/strings/
- Inne możliwości pracy ze stringami w języku Go: https://www.tutorialspoint.com/go/go_string_manipulation.htm