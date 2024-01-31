---
title:                "Fjerne anførselstegn fra en streng"
date:                  2024-01-26T03:39:51.367994-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fjerne anførselstegn fra en streng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å fjerne anførselstegn fra en streng betyr å bli kvitt de irriterende doble eller enkle anførselstegnene som omslutter teksten din. Vi gjør dette for å rense data, forhindre tolkefeil, eller forberede tekst for videre behandling uten den ekstra fyllen av anførselstegn.

## Hvordan:

Her er den enkle måten å sparke disse anførselstegnene til fortauskanten i Go:

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
	quotedString := "\"Hello, World!\""
	fmt.Println("Original:", quotedString)

	unquotedString := removeQuotes(quotedString)
	fmt.Println("Uten anførselstegn:", unquotedString)
}
```

Output vil se slik ut, anførselstegn alle borte:

```
Original: "Hello, World!"
Uten anførselstegn: Hello, World!
```

## Dypdykk

Tilbake i dagen, da dataformater og utveksling ikke var standardisert, kunne anførselstegn i strenger forårsake kaos. De kan fremdeles, spesielt i JSON eller når man dytter strenger inn i databaser. `strings`-pakken i Go kommer lastet med en `Trim`-funksjon, som napper vekk ikke bare mellomrom men også enhver karakter du ikke er fan av.

Hvorfor ikke Regex? Vel, `Trim` er raskere for enkle jobber, men hvis strengene dine leker gjemsel med anførselstegn på rare steder, kan regex være ditt tunge artilleri:

```go
import "regexp"

func removeQuotesWithRegex(s string) string {
	re := regexp.MustCompile(`^["']|["']$`)
	return re.ReplaceAllString(s, "")
}
```

Det er som å velge mellom saks og en motorsag; velg verktøyet som passer for jobben.

## Se også

For mer om `strings`-pakken og dens kraftverktøy:
- [Pakke strings](https://pkg.go.dev/strings)

For å utnytte kraften av regulære uttrykk i Go:
- [Pakke regexp](https://pkg.go.dev/regexp)

Ønsker du å dykke inn i filosofien bak trimming av strenger?
- [Trim-metoden](https://blog.golang.org/strings)
