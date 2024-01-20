---
title:                "Interpolera en sträng"
html_title:           "C++: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad och Varför? 

Att interpolera en sträng i Go-programmering innebär att du infogar en variabels värde direkt i en sträng. Detta gör det möjligt för programmerare att smidigt och dynamiskt manipulera textoutput.

## Hur Man Gör: 

Här är en grundläggande exemplar på stränginterpolation i Go:

```Go
package main
import "fmt"
func main() {
    namn := "Viktor"
    fmt.Println(fmt.Sprintf("Hej, %s!", namn))
}
```

Output:
'''Go
"Hej, Viktor!"
```
'Efter att ha satt värde av "namn" till "Viktor", skriver vi ut en hälsning med hjälp av fmt.Sprintf funktionen, vilket interpolerar "namn" -variabeln in i strängen.' 

## Djupdykning: 

Stränginterpolation har varit en del av programmeringsspråk i årtionden, vilket gör det möjligt att infoga variabler direkt i en sträng över just flera språk. I Go använder vi `fmt.Sprintf` för att uppnå detta, även om liknande metoder finns i andra språk, som `${}` i JavaScript eller `#{}` i Ruby. 

En alternativ metod för att interpolera strängar i Go är att använda `fmt.Printf`,`fmt.Fprintf` eller `fmt.Errorf`. Dessa funktioner skriver till en io.Writer i stället för att de returnerar en sträng.

Ett intressant drag att notera är att Go faktiskt kompilerar stränginterpolation i syntaxträdet som en serie av konkateneringar. Detta innebär att `fmt.Sprintf("Hej %s!", "Viktor")` faktiskt blir `"Hej " + "Viktor" + "!"` under huven.

## Se Också:

För mer läsning om stränginterpolation i Go, kolla in dessa länkar:

[Go by Example: String Formatting](https://gobyexample.com/string-formatting)
[Go Lang Spec: Package fmt](https://golang.org/pkg/fmt/)