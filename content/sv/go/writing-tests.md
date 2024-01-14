---
title:    "Go: Att skriva tester"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av utvecklingsprocessen inom Go-programmering. Det hjälper till att identifiera buggar och förbättra kodens stabilitet, vilket i sin tur leder till en bättre användarupplevelse och minskar risken för fel i produktionen.

## Hur man gör

För att skriva tester i Go behöver du använda paketet "testing" och definiera en funktion som börjar med prefixet "Test" följt av namnet på den funktionen du vill testa. Sedan använder du funktionen "t.Error()" för att markera testet som misslyckat om något går fel, eller "t.Log()" för att logga eventuell information.

```Go
func TestAdd(t *testing.T) {
	result := add(2, 3)
	if result != 5 {
		t.Error("Expected 5, got", result)
	}
	t.Log("TestAdd passed!")
}
```

I detta exempel skapar vi ett enkelt test för en funktion som adderar två tal. Om testet misslyckas kommer meddelandet "Expected 5, got X" att visas, där X är det faktiska resultatet. Annars kommer meddelandet "TestAdd passed!" att loggas.

## Djupdykning

För att skriva effektiva tester, försök att täcka så många fall som möjligt. Detta innefattar hanteringen av gränsfall, felhantering och olika kombinationer av indata. Använd också "Benchmark" funktionen för att mäta prestanda hos din kod och se till att dina tester körs regelbundet för att upptäcka eventuella förändringar i beteendet.

## Se också

* Go Dokumentation: https://golang.org/doc/
* How to Write Go Tests: https://golang.org/src/testing/examples_test.go
* Test Driven Development in Go: https://github.com/quii/learn-go-with-tests