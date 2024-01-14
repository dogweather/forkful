---
title:    "Go: Skriva tester"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/go/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av programmering i Go (Golang). Genom att skriva tester kan du säkerställa att din kod fungerar som den ska och undvika buggar och problem i framtiden. Dessutom hjälper det dig att förstå din kod bättre och gör det enklare att göra ändringar i framtiden.

## Hur man gör det

För att skriva tester i Go behöver du använda paketet "testing". Det innehåller funktioner som gör det möjligt för dig att testa dina funktioner och metoder. Här är ett enkelt exempel på hur man skriver ett test i Go:

```Go
package main

import "testing"

func Add(x, y int) int {
	return x + y
}

func TestAdd(t *testing.T) {
	result := Add(5, 2)
	if result != 7 {
		t.Errorf("Expected 7, but got %d", result)
	}
}
```

I detta exempel har vi en funktion som heter "Add" som lägger till två tal och returnerar resultatet. Vi testar sedan funktionen genom att anropa den och kontrollera att resultatet är korrekt. Om det inte är det, så får vi ett felmeddelande.

Det finns också andra funktioner i paketet "testing" som du kan använda för att testa olika aspekter av din kod, som till exempel "testing.T.Errorf" som används för att rapportera fel.

## Utforska djupare

Att skriva tester handlar inte bara om att skapa enkla fall som ovan. Du kan också utforska andra aspekter av dina funktioner och metoder för att säkerställa att de fungerar korrekt. Det kan inkludera gränsvärden, felhantering och andra scenarier som kan påverka din kod.

En annan viktig del av att skriva tester är att se till att de är enkla att förstå och underhålla. Genom att organisera dina tester på ett logiskt sätt och använda tydliga namn kan du göra det enklare för dig själv och andra utvecklare att förstå din kod och göra ändringar när det behövs.

## Se också

- [Go-paketet "testing"](https://golang.org/pkg/testing/)
- [5 tips for effective testing in Go (engelska)](https://medium.com/@matryer/5-simple-tips-and-tricks-for-writing-unit-tests-in-golang-619653f90742)
- [GopherCon talk: Advanced testing techniques with Go (engelska)](https://www.youtube.com/watch?v=8hQG7QlcLBk&t=333s) ## Se också