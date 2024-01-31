---
title:                "Ta bort citattecken från en sträng"
date:                  2024-01-26T03:39:37.732041-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ta bort citattecken från en sträng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att ta bort citattecken från en sträng innebär att bli av med de där irriterande dubbla eller enkla citattecknen som omsluter din faktiska text. Vi gör detta för att sanera data, förhindra tolkningsfel eller förbereda text för vidare bearbetning utan det extra fluffet av citationstecken.

## Hur man gör:

Här är det enkla sättet att sparka ut dessa citattecken i Go:

```go
package main

import (
	"fmt"
	"strings"
)

func removeQuotes(s string) string {
	return strings.Trim(s, "'\"")
}

func main() {
	quotedString := "\"Hej, världen!\""
	fmt.Println("Original:", quotedString)

	unquotedString := removeQuotes(quotedString)
	fmt.Println("Utan citat:", unquotedString)
}
```

Utmatningen kommer se ut så här, citaten är borta:

```
Original: "Hej, världen!"
Utan citat: Hej, världen!
```

## Djupdykning

Förr i tiden, när dataformat och utbyte inte var standardiserade, kunde citat i strängar ställa till med kaos. De kan fortfarande, speciellt i JSON eller när man matar in strängar i databaser. `strings`-paketet i Go kommer laddat med en `Trim`-funktion, som inte bara tar bort blanksteg, men även andra tecken du inte gillar.

Varför inte Regex? Tja, `Trim` är snabbare för enkla jobb, men om dina strängar bråkar med citat på konstiga platser, kanske regex är ditt tunga artilleri:

```go
import "regexp"

func removeQuotesWithRegex(s string) string {
	re := regexp.MustCompile(`^["']|["']$`)
	return re.ReplaceAllString(s, "")
}
```

Det är som att välja mellan sax och en motorsåg; välj verktyget som passar för jobbet.

## Se även

För mer om `strings`-paketet och dess kraftverktyg:
- [Paket strings](https://pkg.go.dev/strings)

För att hantera kraften av reguljära uttryck i Go:
- [Paket regexp](https://pkg.go.dev/regexp)

Vill du dyka djupare in i filosofin bakom trimning av strängar?
- [Trim-metoden](https://blog.golang.org/strings)
