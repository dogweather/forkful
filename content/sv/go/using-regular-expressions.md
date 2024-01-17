---
title:                "Användning av reguljära uttryck"
html_title:           "Go: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

Vad är reguljära uttryck och varför använder programmerare dem?
Reguljära uttryck, även kända som regex, är en syntax för att beskriva mönster i textsträngar. Programmerare använder regex för att effektivt söka, ersätta och validera data inom textfiler, databaser och mer.
 
Så här använder du reguljära uttryck i Go:
```Go
package main
 
import (
  "fmt"
  "regexp"
)
 
func main() {
  str := "Hej, detta är ett exempel på en textsträng."
  match, _ := regexp.MatchString("exempel", str)
  fmt.Println(match) // output: true
}
```
 
För att kunna använda reguljära uttryck i Go, måste du importera paketet "regexp". Därefter kan du använda funktionen "MatchString" för att söka efter ett visst mönster i en given textsträng. I exemplet ovan letar vi efter ordet "exempel" och då den hittas, returnerar funktionen "MatchString" värdet "true".
 
Fördjupad information:
Reguljära uttryck har funnits sedan 1950-talet och användes ursprungligen för att utföra beräkningar på tidiga datorer. Idag används de flitigt i programmering för att hantera textdata på ett effektivt sätt, istället för att manuellt kontrollera varje enskild teckenkombination.
 
Det finns också andra alternativ för att hantera textdata, till exempel strängmanipulering med hjälp av inbyggda funktioner i programmeringsspråk. Men regex är särskilt användbart när man behöver söka efter mönster som följer ett visst format eller villkor.
 
While reguljära uttryck kan verka krångliga för nybörjare, är det värt att lära sig eftersom de kan spara mycket tid i långa textfiler.
 
Se även:
- Go-dokumentationen för reguljära uttryck: https://golang.org/pkg/regexp/
- Regular-Expressions.info för en omfattande guide till regex: https://www.regular-expressions.info/
- Regex101 för att testa och experimentera med reguljära uttryck: https://regex101.com/