---
title:                "Go: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

I många Go-programmeringsprojekt finns det ofta behov av att manipulera textsträngar för att rensa bort oönskade tecken. Att kunna radera tecken som matchar ett mönster är därför en användbar funktion som kan underlätta både utveckling och optimering av kod.

## Hur man gör det

För att radera tecken som matchar ett visst mönster i en sträng, kan man använda sig av Go:s inbyggda funktion `strings.ReplaceAll()`. Till exempel, om vi vill ta bort alla siffror från en sträng kan vi använda `strings.ReplaceAll(str, "0", "")`. Detta skulle ersätta alla förekomster av "0" med en tom sträng och därmed radera alla siffror från den ursprungliga strängen.

```Go
str := "Det här är en text med siffrorna 123,"
str = strings.ReplaceAll(str, "1", "")
str = strings.ReplaceAll(str, "2", "")
str = strings.ReplaceAll(str, "3", "")

fmt.Println(str) // Output: Det här är en text med siffrorna ,
```

Man kan också använda sig av reguljära uttryck, som är ett mer avancerat sätt att specificera mönster för en sträng. För att radera tecken som matchar ett reguljärt uttryck, kan man använda sig av `regexp.ReplaceAllString()`. Till exempel, om vi vill ta bort alla tecken som inte är bokstäver eller siffror från en sträng, kan vi använda uttrycket `[^\w]` som parameter till funktionen.

```Go
str := "Det här är en text med specialtecken: #%&!"
str = regexp.ReplaceAllString(str, "[^\w]", "")

fmt.Println(str) // Output: Dethärenentextmedspecialtecken
```

## Djupdykning

I Go:s standardbibliotek finns det också andra funktioner som kan användas för att radera tecken från en sträng, såsom `strings.Trim()`, `strings.TrimLeft()`, `strings.TrimRight()` och `strings.TrimPrefix()`. Dessa funktioner tar emot ett set av tecken som ska tas bort från början, slutet eller enbart början eller slutet av en sträng.

En annan viktig punkt att vara medveten om när man raderar tecken från en sträng är att strängar i Go är oföränderliga (immutable), vilket betyder att när man utför en operation på en sträng så skapas en ny sträng istället för att den ursprungliga strängen ändras direkt. Detta kan vara viktigt att tänka på för att undvika överflödiga kopieringar av strängar i minnet när man arbetar med stora datamängder.

## Se även

- Go:s dokumentation om strängfunktioner: https://golang.org/pkg/strings/
- Reguljära uttryck i Go: https://golang.org/pkg/regexp/