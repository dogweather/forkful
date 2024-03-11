---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:06.881604-07:00
description: "Att skriva tester i Go inneb\xE4r att skapa sm\xE5, hanterbara bitar\
  \ av kod som validerar funktionaliteten och beteendet hos din applikation. Programmerare\u2026"
lastmod: '2024-03-11T00:14:10.700896-06:00'
model: gpt-4-0125-preview
summary: "Att skriva tester i Go inneb\xE4r att skapa sm\xE5, hanterbara bitar av\
  \ kod som validerar funktionaliteten och beteendet hos din applikation. Programmerare\u2026"
title: Skriva tester
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva tester i Go innebär att skapa små, hanterbara bitar av kod som validerar funktionaliteten och beteendet hos din applikation. Programmerare skriver tester för att säkerställa att deras kod fungerar som förväntat under olika förhållanden, för att underlätta omstrukturering och för att hjälpa till att förhindra regressioner.

## Hur man gör:

I Go skrivs tester vanligtvis i samma paket som koden de testar. Filer som innehåller tester namnges med suffixet `_test.go`. Tester är funktioner som tar en pekare till testing.T-objektet (från paketet `testing`) som argument, och de signalerar misslyckande genom att anropa metoder som `t.Fail()`, `t.Errorf()`, osv.

Exempel på ett enkelt test för en funktion `Add` definierad i `math.go`:
```go
// math.go
paket math

func Add(x, y int) int {
    return x + y
}
```

Testfil `math_test.go`:
```go
paket math

import "testing"

func TestAdd(t *testing.T) {
    resultat := Add(1, 2)
    förväntat := 3
    if resultat != förväntat {
        t.Errorf("Add(1, 2) = %d; want %d", resultat, förväntat)
    }
}
```

Kör dina tester med kommandot `go test` i samma katalog som dina testfiler. Exempel på utdata som indikerar ett passerat test skulle kunna se ut som:

```
PASS
ok      example.com/my/math 0.002s
```

För tabell-drivna tester, som låter dig effektivt testa olika kombinationer av indata och utdata, definiera en skiva av strukturer som representerar testfall:

```go
func TestAddTableDriven(t *testing.T) {
    var tester = []struct {
        x        int
        y        int
        förväntat int
    }{
        {1, 2, 3},
        {2, 3, 5},
        {-1, -2, -3},
    }

    for _, tt := range tester {
        testnamn := fmt.Sprintf("%d+%d", tt.x, tt.y)
        t.Run(testnamn, func(t *testing.T) {
            svar := Add(tt.x, tt.y)
            if svar != tt.förväntat {
                t.Errorf("fick %d, vill ha %d", svar, tt.förväntat)
            }
        })
    }
}
```

## Fördjupning

Go:s testramverk, som introducerades i Go 1 tillsammans med själva språket, designades för att integreras sömlöst med Go:s verktygskedja, vilket speglar Go:s betoning på enkelhet och effektivitet i programutveckling. Till skillnad från vissa testramverk i andra språk som förlitar sig på externa bibliotek eller komplexa uppsättningar, erbjuder Go:s inbyggda `testing` paket ett okomplicerat sätt att skriva och köra tester.

Ett intressant drag hos Go:s tillvägagångssätt för testning är principen konvention över konfiguration som den antar, liksom filnamnsmönstret (`_test.go`) och användningen av standardbibliotekets funktionaliteter över externa beroenden. Detta minimalistiska tillvägagångssätt uppmuntrar utvecklare att skriva tester, eftersom inträdesbarriären är låg.

Även om Go:s inbyggda testmöjligheter täcker mycket, finns det scenarier där tredjepartsverktyg eller ramverk kan erbjuda mer funktionalitet, som generering av mockobjekt, fuzz-testning eller beteendedriven utveckling (BDD)-stiltester. Populära bibliotek som Testify eller GoMock kompletterar Go:s standardtestkapaciteter och erbjuder mer uttrycksfulla påståenden eller möjligheter för generering av mockobjekt, vilket kan vara särskilt användbart i komplexa applikationer med många beroenden.

Trots existensen av dessa alternativ förblir det standardiserade Go-testpaketet hörnstenen för testning i Go på grund av sin enkelhet, prestanda och nära integration med språket och verktygskedjan. Oavsett om utvecklare väljer att komplettera det med tredjepartsverktyg eller inte, ger Go:s testramverk en solid grund för att säkerställa kodkvalitet och tillförlitlighet.
