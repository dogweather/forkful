---
title:                "Generering av slumpmässiga nummer"
html_title:           "Go: Generering av slumpmässiga nummer"
simple_title:         "Generering av slumpmässiga nummer"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Skapande av slumpmässiga tal är en vanlig uppgift för programmerare. Det innebär helt enkelt att skapa tal som inte följer något specifikt mönster eller regel. Detta är användbart för många olika applikationer, från spelutveckling till kryptering.

## Så här gör du:

Go har ett inbyggt paket för att generera slumpmässiga tal, "math/rand". För att använda det, importera paketet och använd sedan funktionen "Intn()" för att få ut ett slumpmässigt heltal. Exempelvis: 
```
Go import (
	"math/rand"
	"fmt"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Println("Slumpmässigt tal:", rand.Intn(100))
}
```
I det här exemplet kommer "Intn()" att generera ett tal mellan 0 och 99.

## Djupdykning:

Att generera slumpmässiga tal är inte en ny upptäckt. Redan på 1800-talet använde matematiker som Karl Pearson olika metoder för att skapa slumpmässiga tal. Idag finns det olika metoder och algoritmer för att skapa slumpmässiga tal, som t.ex. Middle-Square Method och Linear Congruential Method.

Det finns även alternativ till "math/rand" paketet i Go, som t.ex. "crypto/rand". Detta paket använder kryptografiskt starka algoritmer för att generera slumpmässiga tal, vilket gör det lämpligt för applikationer som kräver hög säkerhet.

För att implementera generering av slumpmässiga tal i dina egna applikationer är det viktigt att förstå de olika metoderna och dess utmaningar. Att använda paket som "math/rand" kan vara enkelt och lättillgängligt, men det kan också leda till vissa mönster och repetitioner i sekvensen av slumpmässiga tal.

## Se även:

https://golang.org/pkg/math/rand/
https://www.geeksforgeeks.org/rand-function-in-golang-with-examples/