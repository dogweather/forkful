---
title:                "Stor bokstaverar en sträng"
html_title:           "Go: Stor bokstaverar en sträng"
simple_title:         "Stor bokstaverar en sträng"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad och Varför?

Att "capitaliza" en sträng betyder att första bokstaven i varje ord ändras till en stor bokstav. Detta är en vanlig praxis bland programmatörer för att förbättra läsbarheten av kod och för att följa konventioner som finns inom programmering.

## Så här gör man:

Go har en inbyggd funktion, kallad ```strings.Title()```, som gör det möjligt att capitaliza en sträng. Det är enkelt att använda, bara skicka in strängen du vill capitaliza som argument och funktionen kommer att returnera den capitalizade strängen.

Exempel:

```
myString := "hej alla programmerare"
fmt.Println(strings.Title(myString))
```

Output:

"Hej Alla Programmerare"

## Djupdykning:

Att capitaliza en sträng är en vanlig praxis inom programmering och har funnits sedan början av datorer och programmeringsspråk. Inom olika programmeringspråk finns det dock olika metoder för att åstadkomma samma sak.

Alternativ till att använda den inbyggda funktionen i Go är att manuellt iterera genom strängen och ändra första bokstaven i varje ord till en stor bokstav. Detta kan dock vara mycket mer tidskrävande och kräver mer kod.

Implementeringsdetaljer för att capitaliza en sträng beror på hur den inbyggda funktionen är implementerad i det specifika programmeringsspråket. Det är vanligt att den använder sig av strängmanipulation och regex för att hitta och ändra första bokstaven i varje ord.

## Se även:

För mer information om strängmanipulation och andra inbyggda funktioner i Go, se officiell dokumentation: https://golang.org/pkg/strings/