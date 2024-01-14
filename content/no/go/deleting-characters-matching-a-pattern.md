---
title:    "Go: Sletting av tegn som matcher et mønster"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har jobbet med tekstbehandling eller dataprogrammering, har du sannsynligvis møtt på situasjonen der du må slette bestemte tegn fra en tekststreng. Dette kan være for å formatere teksten eller fjerne uønskede tegn som ekstra mellomrom eller linjeskift. I Go programming language, er det flere måter å håndtere dette på, og i denne blogginnlegget skal vi se nærmere på hvordan man kan slette tegn som matcher et mønster.

## Slik gjør du det

For å slette tegn som matcher et mønster i Go, kan du bruke standardbibliotekets "strings" pakke og dens "ReplaceAllString" metode. Denne metoden tar inn to argumenter, en tekststreng og et mønster som skal erstattes med en tom streng.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "Dette er en tekststreng som inneholder ekstra mellomrom."
	newText := strings.ReplaceAllString(text, " ", "")
	fmt.Println(newText)

	// Output: Detteerentekststrengsominneholderekstramellomrom.
}
```

Som du kan se, blir alle mellomrommene i tekststrengen slettet, og bare de andre tegnene forblir. Dette kan også gjøres med regex-mønstre ved å bruke "ReplaceAllString" metoden fra "regexp" pakken.

## Dypdykk

Hvis du ønsker mer kontroll over hvordan tegnene blir slettet, kan du bruke "strings" pakken sin "Trim" metode. Denne metoden tar inn to argumenter, en tekststreng og en liste av tegn som skal fjernes fra starten og slutten av strengen.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "!-Dette er en tekststreng som inneholder ekstra mellomrom. "
	newText := strings.Trim(text, " !-")
	fmt.Println(newText)

	// Output: Dette er en tekststreng som inneholder ekstra mellomrom.
}
```

I dette eksemplet blir alle mellomrommene og spesialtegnene fra starten og slutten av teksten fjernet, men mellomrommene inne i teksten forblir uendret.

## Se også

- [Go strings pakke dokumentasjon](https://golang.org/pkg/strings/)
- [Go regexp pakke dokumentasjon](https://golang.org/pkg/regexp/)
- [Golang Playground](https://play.golang.org/)