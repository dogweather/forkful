---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:09.826289-07:00
description: "Att hantera fel i Go inneb\xE4r att k\xE4nna igen och svara p\xE5 felvillkor\
  \ i ditt program. Programmerare \xE4gnar sig \xE5t felhantering f\xF6r att s\xE4\
  kerst\xE4lla att deras\u2026"
lastmod: '2024-03-13T22:44:37.400249-06:00'
model: gpt-4-0125-preview
summary: "Att hantera fel i Go inneb\xE4r att k\xE4nna igen och svara p\xE5 felvillkor\
  \ i ditt program."
title: Hantera fel
weight: 16
---

## Vad och varför?

Att hantera fel i Go innebär att känna igen och svara på felvillkor i ditt program. Programmerare ägnar sig åt felhantering för att säkerställa att deras applikationer kan återhämta sig på ett smidigt sätt från oväntade situationer, vilket leder till mer robust och pålitlig programvara.

## Hur:

I Go hanteras fel explicit med hjälp av `error`-typen. Funktioner som kan misslyckas returnerar ett fel som sitt sista returvärde. Att kontrollera om detta felvärde är `nil` kommer att berätta för dig om ett fel inträffade.

```go
package main

import (
    "errors"
    "fmt"
)

func Compute(value int) (int, error) {
    if value > 100 {
        return 0, errors.New("värdet måste vara 100 eller mindre")
    }
    return value * 2, nil
}

func main() {
    result, err := Compute(150)
    if err != nil {
        fmt.Println("Fel:", err)
    } else {
        fmt.Println("Resultat:", result)
    }
    
    // Att hantera ett fel smidigt
    ettAnnatResultat, ettAnnatFel := Compute(50)
    if ettAnnatFel != nil {
        fmt.Println("Fel:", ettAnnatFel)
    } else {
        fmt.Println("Resultat:", ettAnnatResultat)
    }
}
```

Exempelutdata för koden ovan:
```
Fel: värdet måste vara 100 eller mindre
Resultat: 100
```

I det här exemplet returnerar `Compute`-funktionen antingen ett beräknat värde eller ett fel. Anroparen hanterar felet genom att kontrollera om `err` inte är `nil`.

## Fördjupning

Gos tillvägagångssätt för felhantering är med avsikt enkelt och typsäkert, vilket kräver explicita kontroller av fel. Detta koncept kontrasterar med undantagsbaserad felhantering som ses i språk som Java och Python, där fel propageras upp genom anropsstacken om de inte fångas av en undantagshanterare. Go-teamet argumenterar för att den explicita hanteringen av fel resulterar i klarare och mer tillförlitlig kod, eftersom det tvingar programmerare att omedelbart ta itu med fel där de inträffar.

Dock nämner somliga kritiker att detta mönster kan leda till utförlig kod, särskilt i komplexa funktioner med många felbenägna operationer. Som svar har nyare versioner av Go infört mer sofistikerade funktioner för felhantering, såsom felomslagning, vilket gör det lättare att ge sammanhang till ett fel utan att förlora den ursprungliga felinformationen. Gemenskapen har också sett förslag på nya mekanismer för felhantering, såsom kontroll/hantera, även om dessa fortfarande diskuteras vid min senaste uppdatering.

Gos filosofi för felhantering betonar förståelse och planering för fel som en del av programmets normala flöde. Detta tillvägagångssätt uppmuntrar utvecklingen av mer resilient och förutsägbar programvara, om än med en potentiell ökning i boilerplate-kod. Alternativa mönster och bibliotek finns för att effektivisera felhantering för särskilt komplexa fall, men Go:s inbyggda `error`-typ förblir grundvalen för felhantering i språket.
