---
title:                "Att starta ett nytt projekt"
date:                  2024-02-03T18:09:26.853368-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att starta ett nytt projekt"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/starting-a-new-project.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att starta ett nytt projekt i Go innebär att sätta upp en arbetsyta och initiera den med nödvändiga Go-moduler. Programmerare gör detta för att organisera kod, hantera beroenden effektivt och underlätta byggprocesser. Det är grundläggande för att skapa skalbar och underhållbar programvara i Go.

## Hur man gör:

Först, se till att du har Go installerat genom att köra `go version` i din terminal. Du bör se den version av Go som du har installerat som utdata. Nästa steg är att starta ett nytt projekt. Navigera till din arbetsyta och kör:

```shell
mkdir hello-world
cd hello-world
```

Detta skapar och flyttar dig till en ny katalog för ditt projekt. Initiera nu modulen:

```shell
go mod init example.com/hello-world
```

Byt ut `example.com/hello-world` mot din modulsökväg. Detta kommando skapar en `go.mod`-fil i din katalog, vilket signalerar starten på en ny Go-modul. Så här kan `go.mod` se ut:

```plaintext
module example.com/hello-world

go 1.18
```

`go.mod` spårar ditt projekts beroenden. Skapa nu en `main.go`-fil:

```shell
touch main.go
```

Öppna `main.go` i din favoriteditor och lägg till följande kod för att skriva ut "Hello, World!":

```go
package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
}
```

För att köra ditt program, navigera tillbaka till terminalen och exekvera:

```shell
go run main.go
```

Du bör se:

```plaintext
Hello, World!
```

Grattis! Du har precis startat ett nytt Go-projekt och kört ditt första Go-program.

## Fördjupning

Initiativet att introducera moduler som standarden för hantering av beroenden i Go var en betydande förändring i Go-ekosystemet, officiellt antaget i Go 1.11. Innan moduler förlitade sig Go-utvecklare på miljövariabeln GOPATH för att hantera beroenden, vilket var mindre intuitivt och ofta ledde till det ökända “beroendehelvetet”.

Moduler tillhandahåller ett inkapslat sätt att hantera projekts beroenden, versionering och är ett steg mot att göra Go-projekt mer självständiga och portabla. Varje modul specificerar sina beroenden som Go spårar i `go.mod`-filen, vilket förenklar hanteringen av beroenden över olika miljöer och utvecklingsstadier.

Det är dock värt att notera att även om Go-moduler nu är standarden, kan vissa äldre projekt fortfarande använda GOPATH. För de flesta nya projekt erbjuder moduler ett enklare och mer effektivt hanteringssystem, men att förstå GOPATH kan vara praktiskt för att underhålla eller bidra till äldre Go-kodbaser.

När det gäller alternativ, även om Go-moduler nu är den de facto standarden, har Go-gemenskapen experimenterat med andra verktyg för hantering av beroenden som `dep` i det förflutna. Dessa har dock till stor del överträffats av det officiella modulstödet integrerat i Go-verktygskedjan.
