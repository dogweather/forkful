---
title:                "Utvinna delsträngar"
html_title:           "Go: Utvinna delsträngar"
simple_title:         "Utvinna delsträngar"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

När vi pratar om att extrahera substrängar i programmeringsspråket Go, menar vi att hämta en del av en befintlig sträng och lagra den som en egen variabel. Detta är användbart när vi behöver hantera större strängar och endast behöver arbeta med vissa delar av dem. Programmerare använder detta till exempel när de ska sortera, filtrera eller jämföra data.

## Så här gör du:

```Go
package main

import (
	"fmt"
)

func main() {
	str := "Detta är en sträng för att demonstrera substrängsextraktion."
	substr := str[5:18]
	fmt.Println(substr)
}
```
Output: ```är en sträng```

Här visar vi ett exempel på hur man extraherar en substräng från en befintlig sträng i Go. Vi deklarerar en variabel ```str``` med en längre sträng som innehåller texten vi vill arbeta med. Sedan får vi tillgång till en del av denna sträng genom att använda index och kolon för att markera vilka tecken vi vill extrahera. I det här fallet vill vi ha tecken från den sjätte positionen (där index 0 är den första positionen) till den artonde positionen, vilket ger oss substrängen "är en sträng".

## Djupdykning:

Historiskt sett har substrängsextraktion ansetts vara en av de mer ineffektiva manipulationerna av textsträngar. Detta på grund av att det är en relativt dyr operation jämfört med andra textmanipulationsmetoder. Det finns dock alternativ till substrängsextraktion, som till exempel att använda reguljära uttryck eller att dela upp strängen i mindre delar med hjälp av t.ex. split-funktionen.

I Go implementerades substrängsextraktion genom att låta index- och long operatorn fungera på strängar. Det finns även en inbyggd standardbiblioteksfunktion för substrängsextraktion, ```strings.SubString()```, som kan användas för att extrahera substrängar på olika sätt.

## Se även:

- [Officiell dokumentation för Go om substrängsextraktion](https://golang.org/ref/spec#Slice_expressions)
- [En guide om Go strängoperationer](https://www.golang-book.com/books/intro/7#section2)