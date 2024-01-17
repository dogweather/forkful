---
title:                "Att skriva tester"
html_title:           "Go: Att skriva tester"
simple_title:         "Att skriva tester"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva tester är en viktig del av programmering. Det är en metod för att säkerställa kvaliteten på din kod genom att köra den genom olika scenarier och kontrollera att den fungerar som förväntat. Detta hjälper till att upptäcka och åtgärda fel och bidrar till att säkerställa att din kod är robust och pålitlig.

## Hur gör man?

För att skriva tester i Go använder man "testing" paketet som ingår i standardbiblioteket. I detta paket finns funktioner, som "testing.T" som möjliggör för oss att skapa och köra tester. Nedan följer ett exempel på hur man kan göra en enkel "pass" test:

```Go
package main

import (
	"testing"
)

func multiply(x int, y int) int {
	return x * y
}

func TestMultiply(t *testing.T) {
	expected := 10
	a := 2
	b := 5
	actual := multiply(a, b)
	if actual != expected {
		t.Errorf("Expected %d, but got %d", expected, actual)
	}
}
```

Återigen, detta är bara ett enkelt exempel för att visa hur tester kan skrivas i Go. Det finns många olika sätt att skriva tester och det beror på din kod och dina specifika behov.

## Djupdykning

Historiskt sett har väl skrivna tester varit en del av kvalitets- och testdriven utveckling (TDD) metoder. TDD innebär att man skriver tester först och sedan utvecklar koden för att uppfylla de tester man skrivit. Alternativet till TDD är att skriva tester först efter att man skrivit sin kod, vilket kallas för testning efter kod (BDD).

Utöver Go's standard testing paket finns det också andra tester verktyg som kan vara bra att känna till, som till exempel "goconvey" och "ginkgo". Dessa erbjuder mer avancerade funktioner och möjligheter att organisera och strukturera dina tester.

## Se även

- [Go's Testing Paket Dokumentation](https://golang.org/pkg/testing/)
- [En bra introduktion till testning i Go](https://medium.com/@thedevsaddam/testing-golang-i-go-74536b6e8d48)
- [Mer information om testdriven utveckling och testning efter kod](https://martinfowler.com/bliki/TestDrivenDevelopment.html)
- [goconvey paketet](https://github.com/smartystreets/goconvey)
- [ginkgo paketet](https://github.com/onsi/ginkgo)