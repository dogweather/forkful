---
title:                "Sökning och ersättning av text"
html_title:           "Arduino: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att söka och ersätta text är en vanlig programmeringsaktivitet där ett eller flera förekomster av en specifik textsträng byts ut mot en annan. Det är särskilt användbart för databehandling, att automatisera korrigeringar och för att förbättra kodrensning.

## Så Här Gör Du:

I Go kan du enkelt använda `strings` paketet för att söka och ersätta text. Här är ett exempel:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	mytext := "Hej, jag älskar Go programmering!"
	replacedText := strings.Replace(mytext, "Go", "Python", -1)
	fmt.Println(replacedText)
}
```

När du kör detta program, kommer output att vara:

```
Hej, jag älskar Python programmering!
```

## Fördjupning:
Sedan antiken har människor letat efter sätt att söka och byta ut text. Det kanske började med att korrigera misstag i manuskript, men i den digitala eran har denna aktivitet blivit en integrerad del i programmering och datahantering.

När vi pratar om alternativ till Go, erbjuder flera programmeringsspråk som Python, Java och JavaScript inbyggda metoder för textersättning. Men Go skiljer sig ut med sin prestanda och enkel mekanism, vilket är en väsentlig fördel, särskilt vid behandling av stora datamängder.

Från implementationssynpunkt använder Go's `strings.Replace` metoden en enkel iterationsprocess. Den går igenom textsträngen från vänster till höger och byter ut alla förekomster av söksträngen med ersättande strängen. Parametern `-1` betyder att den ersätter alla förekomster.

## Se Även:

1. Go's officiella dokumentation: [https://golang.org/pkg/strings/#Replace](https://golang.org/pkg/strings/#Replace)
4. En tutorial på att använda JavaScripts `replace` funktion: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)