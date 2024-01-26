---
title:                "Refaktorisering"
date:                  2024-01-26T01:18:45.052299-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorisering"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/refactoring.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Refaktorisering är processen att omstrukturera befintlig dator kod utan att ändra dess externa beteende. Programmerare gör detta för att förbättra programvarans icke-funktionella attribut, som läsbarhet och underhållbarhet, vilket kan göra koden lättare att förstå, reducera komplexitet och hjälpa till att upptäcka buggar mer lätt.

## Hur man gör:
Låt oss dyka in i ett enkelt exempel på refaktorisering av Go-kod. Vi tar ett kodstycke som beräknar medelvärdet av en slice av nummer och refaktoriserar det för klarhet och återanvändbarhet.

Originalkod:
```Go
package main

import "fmt"

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    var sum float64
    for _, num := range numbers {
        sum += num
    }
    average := sum / float64(len(numbers))
    fmt.Println("Medelvärde:", average)
}
```

Refaktoriserad kod:
```Go
package main

import "fmt"

// CalculateAverage tar en slice av float64 och returnerar medelvärdet.
func CalculateAverage(numbers []float64) float64 {
    sum := 0.0
    for _, num := range numbers {
        sum += num
    }
    return sum / float64(len(numbers))
}

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    average := CalculateAverage(numbers)
    fmt.Println("Medelvärde:", average)
}
```

I den refaktoriserade koden har vi extraherat logiken som beräknar medelvärdet till en separat funktion vid namn `CalculateAverage`. Detta gör `main`-funktionen mer koncis och logiken för beräkning av medelvärdet återanvändbar och testbar.

## Fördjupning
Refaktorisering av kod är inte ett modernt koncept; det fanns innan omfattande användning av datorer. Praktiken började troligtvis inom området mekanisk ingenjörskonst eller ännu tidigare. Inom mjukvara blev det mer formaliserat med framväxten av objektorienterad programmering och extrem programmering (XP) på 1990-talet, märkbart påverkat av Martin Fowlers banbrytande bok "Refactoring: Improving the Design of Existing Code."

Det finns många refaktoreringstekniker, från enkelt att döpa om variabler för klarhet till mer komplexa mönster som att extrahera metoder eller klasser. Nyckeln är att göra små, stegvisa förändringar som inte modifierar programmets funktionalitet men förbättrar den interna strukturen.

När man använder Go kan refaktorisering vara rättfram på grund av språkets enkelhet och kraftfulla standardbibliotek. Det är dock fortfarande viktigt att ha ett bra set av enhetstester för att säkerställa att refaktoriseringen inte introducerar buggar. Verktyg som `gorename` och `gofmt` hjälper till att automatisera en del av processen, och IDE:er har ofta inbyggt stöd för refaktorisering.

Utöver manuell refaktorisering finns det några automatiserade verktyg för kodrefaktorisering tillgängliga för Go, som GoLands refaktoreringsverktyg och Go Refactor. Även om dessa kan påskynda processen, ersätter de inte att förstå koden och göra genomtänkta förändringar.

## Se även
 - [Refaktorisering i Go: Enkelt är vackert](https://go.dev/blog/slices)
 - [Effektiv Go: Refaktorisering med gränssnitt](https://go.dev/doc/effective_go#interfaces)
 - [Martin Fowlers refaktoreringssida](https://refactoring.com/)
 - [GoLand Refaktoreringsverktyg](https://www.jetbrains.com/go/features/refactorings/)