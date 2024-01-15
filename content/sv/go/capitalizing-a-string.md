---
title:                "Kapitalisera en sträng"
html_title:           "Go: Kapitalisera en sträng"
simple_title:         "Kapitalisera en sträng"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Hormonerna är sannolikt skyhöga och det är dags att sätta på sig den rosa Go-tröjan för vi ska prata om hur man kapitaliserar en sträng i Go! Det kanske låter tråkigt, men det är faktiskt en viktig färdighet som kommer att hjälpa dig att skriva bättre kod och underlätta kommunikationen med andra utvecklare.

## Varför

Det finns många olika anledningar till varför man skulle vilja kapitalisera en sträng i Go. Till exempel kan det vara för att matcha en specifik namngivningskonvention, kommunicera tydligt med användare eller helt enkelt för att göra koden lättare att läsa och förstå. Oavsett anledning är det en nyttig färdighet att ha i verktygslådan.

## Hur man gör det

Att kapitalisera en sträng i Go är relativt enkelt. Du kan använda den inbyggda funktionen "strings.Title" för att göra det direkt. Här är ett exempel på hur det kan se ut i kod:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "hej världen"
	fmt.Println(strings.Title(str))
}
```

Detta kommer att ge utmatningen "Hej Världen", vilket är en kapitaliserad version av strängen "hej världen". Du kan också använda "strings.ToUpper" för att bara göra första bokstaven i strängen stor.

## Djupdykning

Nu när vi har täckt det grundläggande, låt oss titta lite närmare på hur funktionen "strings.Title" faktiskt fungerar. Den tar en sträng som inmatning och returnerar en kopia av den strängen med första bokstaven i varje ord kapitaliserad. Detta innebär att om strängen innehåller siffror eller specialtecken, kommer de att förbli opåverkade.

Det är viktigt att komma ihåg att språket Go är "byte-stabilt". Det betyder att du inte kan ändra en sträng direkt, du måste skapa en ny kopia med de ändringar du vill göra, vilket är vad "strings.Title" funktionen gör.

## Se även

- [Official Go Documentation on strings package](https://golang.org/pkg/strings/)
- [GopherCon talk: The Strings Package in Go](https://www.youtube.com/watch?v=0Wts1Cm8WV0)
- [Medium article: The Hidden Power of Strings in Go](https://medium.com/@agathver/the-hidden-power-of-strings-in-go-91c5c7abb8f1)