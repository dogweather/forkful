---
title:                "Go: Läsa kommandoradsargument"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför
Att läsa argument från kommandoraden är en viktig del av att utveckla program i Go. Det ger dig möjlighet att få input från användaren och anpassa programmets beteende baserat på detta. I denna bloggpost kommer vi att gå igenom hur du läser kommandoradsargument på ett enkelt och effektivt sätt i Go.

## Hur man gör det
För att läsa kommandoradsargument i Go, behöver du använda paketet "flag". Du kan börja genom att importera paketet och definiera de variabler som du vill att kommandoradsargumenten ska tilldelas till.

```
import "flag"

var username string
var age int
```

Därefter kan du använda funktionen "flag.String" eller "flag.Int" för att läsa in argumenten och tilldela dem till variablerna.

```
flag.StringVar(&username, "u", "", "Användarnamn")
flag.IntVar(&age, "a", 0, "Ålder")
```

I detta exempel betyder "u" och "a" att dessa argument kan skrivas in från kommandoraden efter flaggan "-u" eller "-a". Om du till exempel vill läsa in användarnamnet "Lisa" och åldern 25, kan du köra programmet på följande sätt: "go run main.go -u Lisa -a 25".

Nu har variablerna "username" och "age" blivit tilldelade med dessa värden och du kan använda dem i ditt program.

```
fmt.Printf("Användare: %s\n", username)
fmt.Printf("Ålder: %d\n", age)
```

Med detta enkla exempel kan du nu läsa in kommandoradsargument och använda dem i ditt Go-program.

## Djupdykning
I det föregående exemplet använde vi flaggor för att definiera vilka argument som ska läsas in. Det finns också andra alternativ som kan hjälpa dig att hantera kommandoradsargument mer flexibelt, såsom att läsa in flera argument och hantera olika datatyper. Du kan läsa mer om dessa funktioner i Golangs officiella dokumentation [här](https://golang.org/pkg/flag/).

## Se även
- [Golangs officiella dokumentation om flag-paketet](https://golang.org/pkg/flag/)
- [En tutorial om läsning av kommandoradsargument i Go](https://www.digitalocean.com/community/tutorials/how-to-read-command-line-arguments-in-golang)
- [En video tutorial om hantering av kommandoradsargument i Go](https://www.youtube.com/watch?v=nutbDp_tXYs)