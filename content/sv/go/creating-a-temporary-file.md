---
title:                "Go: Skapa en tillfällig fil"
simple_title:         "Skapa en tillfällig fil"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Ibland när vi programmerar, behöver vi skapa temporära filer för att hålla data eller utföra vissa operationer. Det kan vara för att spara temporärt data från ett API-anrop eller för att göra beräkningar som inte behöver sparas permanent. Oavsett anledning, så är det viktigt att veta hur man skapar en temporär fil i Go.

## Så här

Att skapa en temporär fil i Go är ganska enkelt. Vi behöver först importera "io/ioutil" paketet för att ha tillgång till funktionen "TempFile" som låter oss skapa filer i det temporära katalogen på vår dator. Vi kan sedan använda denna funktion för att skapa filen och få en "File" typ tillbaka som vi kan använda för att läsa och skriva data till filen.

```Go
import "io/ioutil"

tempFile, err := ioutil.TempFile("", "tempfile")
if err != nil {
    panic(err)
}
defer tempFile.Close()

fmt.Println("Temporär fil skapad:", tempFile.Name())
// Output: Temporär fil skapad: C:\Users\user\AppData\Local\Temp\tempfile821895406

tempFile.Write([]byte("Det här är en temporär fil"))
data, _ := ioutil.ReadFile(tempFile.Name())
fmt.Println("Innehåll i filen:", string(data))
// Output: Innehåll i filen: Det här är en temporär fil
```

Som vi kan se i exemplet ovan, använder vi "TempFile" funktionen för att skapa en temporär fil med namnet "tempfile". Vi använder sedan "Write" funktionen för att skriva data till filen och "ReadFile" funktionen för att läsa data från filen. Slutligen stänger vi filen med hjälp av "Close" funktionen och får tillbaka namnet på den skapade filen med hjälp av "Name" funktionen.

## Djupdykning

För de som är intresserade av att veta mer om att skapa temporära filer i Go, så är det värt att notera att "TempFile" funktionen tar två parametrar. Den första är för prefixet som används för filnamnet och den andra är för att ange en specifik katalog att skapa filen i. Om den andra parametern lämnas tom, så skapas filen i det temporära katalogen på datorn. Detta kan vara användbart om vi vill hålla våra temporära filer organiserade.

## Se också

* [Go dokumentation - TempFile funktionen](https://pkg.go.dev/io/ioutil#TempFile)
* [Go dokumentation - io/ioutil paketet](https://golang.org/pkg/io/ioutil/)
* [Go exempel - TempFile](https://gobyexample.com/temporary-files)