---
title:                "Go: Omvandling av en sträng till små bokstäver"
simple_title:         "Omvandling av en sträng till små bokstäver"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till små bokstäver kan vara ett vanligt problem för programmerare, särskilt när man hanterar användarinput. Detta kan hjälpa till att säkerställa att jämförelser eller validering av inmatad data blir mer korrekt och enhetlig.

## Så här gör du

För att konvertera en sträng till små bokstäver i Go, kan du använda den inbyggda funktionen "ToLower" från paketet "strings". Här är ett enkelt exempel:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "HEJ VAD HETER DU?"
	fmt.Println(strings.ToLower(str))
}

```

Detta program kommer att skriva ut "hej vad heter du?" som output. Som du kan se använde vi bara "strings.ToLower" för att konvertera vår sträng.

Om du vill göra en mer avancerad konvertering, kan du också använda "strings.Map" funktionen från samma paket. Här är ett exempel där vi konverterar varje vokal i en sträng till en stor bokstav:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "hej vad heter du?"
	fmt.Println(strings.Map(func(r rune) rune {
		if strings.Contains("aeiouy", string(r)) {
			return r - 32 //konvertera till stor bokstav
		}
		return r
	}, str))
}

```

Output: "Hej Vad HETEr dU?" I detta exempel använde vi funktionen "strings.Contains" för att kontrollera om tecknet är en vokal, sedan återvände vi en modifierad version av strängen.

## Djupdykning

När det gäller att konvertera en sträng till små bokstäver kan det finnas vissa fall där standardfunktionerna inte fungerar som förväntat. Det kan bero på språk-kodning, speciella tecken eller annat. I sådana fall kan det vara bra att använda "strings.ToLowerSpecial" eller "strings.MapUnicode" för att hantera konverteringen på ett annat sätt.

En annan viktig aspekt att tänka på är hur konverteringen av tecken sker. I Go, som stöder Unicode-tecken, bör man vara medveten om att konvertering kan ändra antalet tecken i en sträng, beroende på vilka tecken som finns i den. Detta kan vara viktigt vid behandling av användardata, särskilt om längden av strängen är avgörande.

## Se även

- [Go strings-paketet](https://golang.org/pkg/strings/)
- [Go strings.Map-funktionen](https://golang.org/pkg/strings/#Map)
- [Go Unicode-support](https://blog.golang.org/strings)

Tack för att du läste! Hoppas att denna guide har varit till hjälp i ditt arbete med att konvertera strängar till små bokstäver i Go. Lycka till med dina framtida projekt!