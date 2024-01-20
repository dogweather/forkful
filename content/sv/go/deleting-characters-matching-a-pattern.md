---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Arduino: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att ta bort tecken som matchar ett mönster i Go betyder att filtrera ut några specifika tecken i en sträng. Programmerare gör detta för att manipulera data eller få text i önskat format.

## Så här gör du:
Här är hur du tar bort en sträng med ett matchande mönster i Go.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Denna sträng innehåller några tecken att ta bort"
	if strings.Contains(str, "att ta bort") {
		str = strings.Replace(str, "att ta bort", "", -1)
	}
	fmt.Println(str)
}
```

Det här scriptet kommer att producera: 

```
"Denna sträng innehåller några tecken"
```

## Djupdykning
Metoden vi just använt, `strings.Replace`, i Go har historiskta rötter tillbaka till det klassiska språket C. Alternativ till `strings.Replace` inkluderar användning av paketet `regexp` för mer komplexa mönstermatchningar. Ytterligare en metod är att konvertera strängen till ett rune-arrangemang och sedan iterera över det och urvalet.

Implementationsdetaljer att notera inkluderar att `strings.Replace` första argument är den ursprungliga strängen, det andra argumentet är det mönster vi vill ersätta, det tredje argumentet är den nya strängen vi vill ersätta med, och det fjärde argumentet är antalet gånger vi vill utföra ersättningen - en negativ siffra kommer att ersätta alla förekomster.

## Se även
Relaterade länkar se [här](https://golang.org/pkg/strings/#ReplaceAll) för Go dokumentation om 'strings.Replace' och [här](https://golang.org/pkg/regexp/) för mer information om Go's 'regexp' paket.