---
title:                "Go: Extrahering av delsträngar"
simple_title:         "Extrahering av delsträngar"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför
Substring-extrahering är en mycket användbar funktion i Go som gör det möjligt för oss att plocka ut en del av en sträng baserat på ett visst villkor. Detta kan vara användbart för att bearbeta text eller för att söka igenom stora mängder data.

## Så här gör du
För att extrahera substrings i Go, använder vi funktionen `strings.Substring()` som är tillgänglig genom `strings`-paketet. När vi använder denna funktion, måste vi tillhandahålla två parametrar: den ursprungliga strängen och indexpositionen för den del av strängen som vi vill extrahera.

Låt oss se ett exempel där vi vill extrahera de första fem tecknen från en sträng "Hej världen". Detta skulle se ut så här i kod:

```Go
str := "Hej världen"
substr := str[0:5]
fmt.Println(substr) // Skriver ut "Hej v"
```

Som vi kan se använder vi index från 0 till 5 för att extrahera de första fem tecknen från strängen.

När det gäller att extrahera substrings från en viss position, kan vi också använda funktionen `strings.Index()` för att hitta positionen för ett specifikt tecken eller en specifik sträng för att sedan använda detta som index i `strings.Substring()`.

Låt oss se ett annat exempel där vi vill extrahera texten efter "@" i en e-postadress:

```Go
str := "example@gmail.com"
index := strings.Index(str, "@") + 1 
substr := str[index:]
fmt.Println(substr) // Skriver ut "gmail.com"
```

Genom att använda `strings.Index()` hittar vi positionen för "@" och lägger sedan till 1 för att hoppa över det tecknet och extrahera resten av strängen.

## Djupdykning
Det finns flera anledningar till att extrahera substrings kan vara användbart. Det kan till exempel användas för att kontrollera giltigheten av e-postadresser, extrahera användarnamn från inloggning eller för att hantera sökningar i en databas.

Förutom användandet av `strings.Substring()` och `strings.Index()`, finns det också andra funktioner och metoder som kan användas för att extrahera substrings i Go, såsom `strings.Split()` och `regexp`-paketet som erbjuder mer avancerade sökmöjligheter.

## Se även
- [Go Strings Package](https://golang.org/pkg/strings/)
- [Regular Expressions in Go](https://golang.org/pkg/regexp/)