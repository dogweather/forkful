---
title:                "Go: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför: Varför du bör använda debug-utskrift i ditt Go-programmering.

För att lösa problem och felsöka koden är det ofta nödvändigt att få utskrifter av variabler och steg i koden. Detta hjälper till att förstå vad som händer i koden och var eventuella fel uppstår. Det kan också vara användbart för att få en överblick över en komplex kodbas.

## Hur man gör: Kodexempel och utskriftsresultat inuti kodblock med "```Go ... ```"

För att skriva ut ett värde i Go använder man funktionen "fmt.Println()". Till exempel:

```Go
package main

import "fmt"

func main() {
  name := "Anna"
  fmt.Println("Hej", name)
  // Utskrift: Hej Anna
}
```

För att använda variabler i utskrifter, använd "%v" i strängen och placera sedan variabeln efteråt inuti en annan parentes. Till exempel:

```Go
package main

import "fmt"

func main() {
  age := 27
  fmt.Println("Jag är %v år gammal.", age)
  // Utskrift: Jag är 27 år gammal.
}
```

För att få ut mer information om en variabel, kan man använda "%T" för att få ut datatypen och "%#v" för att få ut hela variabeln med dess namn och värde inuti en parentes. Till exempel:

```Go
package main

import "fmt"

func main() {
  number := 5
  fmt.Printf("%T\n", number)
  // Utskrift: int

  fmt.Printf("%#v\n", number)
  // Utskrift: 5
}
```

## Djupdykning: Mer information om debug-utskrift i Go

Det finns olika funktioner för debug-utskrifter i Go utöver "fmt.Println()". Till exempel kan man använda "log.Println()" för att få utskrifter som inkluderar datum och tid, och "fmt.Sprintf()" för att spara utskriftsresultatet i en variabel istället för att skriva ut det direkt.

Det finns också olika formatteringsalternativ som "%d" för heltal och "%f" för flyttal. Mer information om dessa och andra funktioner för debug-utskrift i Go finns i dokumentationen för "fmt" paketet.

## Se också

- [Dokumentation för "fmt" paketet](https://golang.org/pkg/fmt/)
- [Go Språkspecifikation](https://golang.org/ref/spec)