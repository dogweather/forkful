---
title:    "Go: Läsning av kommandoradsargument"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Varför

I denna bloggpost kommer vi att diskutera hur man kan läsa in kommandoradsargument i Go-programmeringsspråket. Att kunna läsa och behandla kommandoradsargument är en viktig del av att bygga robusta och användarvänliga exekverbara filer.

## Hur man gör

Först och främst behöver vi importera os-paketet för att kunna använda inbyggda funktioner som hjälper oss att läsa in kommandoradsargument. Sedan skapar vi en funktion för att läsa in kommandoradsargument och hantera eventuella fel.

```Go
import "os"

func readArgs() {
    args := os.Args[1:]

    for _, arg := range args {
        fmt.Println(arg)
    }
}
```

För att testa vår funktion kan vi köra vårt program från kommandoraden och ange några argument. Till exempel:

```
go run main.go argument1 argument2
```

Detta kommer att skriva ut argumenten som vi har gett som ingång till programmet:

```
argument1
argument2
```

Vi kan även ta emot flaggor som argument och behandla dem på lämpligt sätt. Till exempel kan vi använda flag-paketet för att förvänta oss vissa flaggor och utföra olika handlingar beroende på vilka flaggor som ges som argument.

## Deep Dive

Kommandoradsargument är också användbara för att specificera filvägar eller andra indata då det inte är möjligt att ange dessa direkt i källkoden. Detta kan vara särskilt användbart när man arbetar med stora mängder data eller genererar filer som ska användas av andra program.

Det finns också andra sätt att hantera kommandoradsargument, till exempel genom att använda regex för att matcha specifika mönster eller genom att använda tredjepartsbibliotek för att ge mer avancerad funktionalitet.

Det är viktigt att notera att läsning av kommandoradsargument inte är en komplett lösning för felhantering och det är fortfarande viktigt att validera och hantera användarindata på ett säkert sätt.

## Se även

Här är några användbara länkar för mer information om att läsa kommandoradsargument i Go:

- [Officiell dokumentation för os-paketet](https://golang.org/pkg/os/)
- [Officiell dokumentation för flag-paketet](https://golang.org/pkg/flag/)
- [Användbara tips för att hantera kommandoradsargument i Go](https://peter.bourgon.org/blog/2019/02/12/command-line-arguments.html)