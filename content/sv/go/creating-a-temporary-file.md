---
title:                "Go: Skapa en tillfällig fil"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Att skapa tillfälliga filer är en vanlig process inom programmering. De används för att temporärt lagra data eller för att tillfälligt hantera filer som annars skulle ta upp mycket utrymme. Att kunna skapa en tillfällig fil kan vara en viktig del av många program, och det är därför värt att lära sig hur man gör det i Go.

## Hur man gör det

För att skapa en temporär fil i Go använder man funktionen "TempFile" från paketet "ioutil". Den här funktionen tar två argument - första är sökvägen där filen ska skapas och andra är prefixet för filnamnet. Om sökvägen lämnas som tom kommer filen att skapas i standardmapp för temporära filer. Prefixet är bara en sträng som används som en del av filnamnet, till exempel "tempfile". Det är också möjligt att lämna både sökvägen och prefixet som tomma och i det fallet kommer filen att skapas i standardmappen med ett slumpmässigt namn som prefix.

För att använda "TempFile" behöver man importera paketet "ioutil":
```Go
import "io/ioutil"
```

För att skapa en temporär fil i standardmappen används följande kod:
```Go
tempFile, err := ioutil.TempFile("", "tempfile")
if err != nil {
    // hantera fel
}
defer os.Remove(tempFile.Name()) // ta bort filen när den inte längre behövs
```

Kör man koden ovan kommer en ny fil att skapas i standardmappen för temporära filer, med namnet "tempfile" och ett unikt slumpmässigt nummer som suffix. Om man vill att filen ska skapas i en specifik mapp kan man ange den mappen som första argument till "TempFile", till exempel "C:/temp". Det är också möjligt att lämna sökvägen tom och endast ange prefixet för att få en unik fil i standardmappen.

## Deep Dive

Funktionen "TempFile" skapar inte bara en temporär fil, utan den öppnar också filen för läs och skrivning. Det är också möjligt att ange ett tredje argument till funktionen för att ställa in filens behörighet. Om man till exempel vill att filen endast ska gå att läsa av den process som skapade den kan man använda följande kod:
```Go
tempFile, err := ioutil.TempFile("", "tempfile", 0600) // behörighet sätts till 600
```

Det finns också en motsvarande funktion "TempDir" som används för att skapa tillfälliga mappar istället för filer. Den fungerar på samma sätt som "TempFile" och har även en tredje parameter för att ställa in behörighet.

## Se även

Här är några användbara länkar för mer information om att skapa temporära filer i Go:

- [Go's officiella dokumentation för "ioutil" paketet](https://golang.org/pkg/io/ioutil/)
- [Mer om att skapa temporära filer i Go på Stack Overflow](https://stackoverflow.com/questions/39824125/creating-temporary-file-in-golang)
- [En översikt av "ioutil" paketet på Go by Examples](https://gobyexample.com/writing-files)