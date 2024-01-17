---
title:                "Läsa en textfil"
html_title:           "Go: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Läsning av en textfil är ett sätt för programmet att ta in information från en fil och sedan använda den i programkoden. Många gånger behöver programmerare använda information från en textfil för att hantera data eller konfigurationsinställningar i deras program.

## Hur man:
Go har ett enkelt sätt att läsa en textfil. Använd funktionen "os.Open ()" för att öppna textfilen och sedan läsa in den med "bufio.NewScanner ()". Detta skapar en scanner som går igenom filen rad för rad och du kan använda en for-loop för att läsa varje rad. Sedan kan du använda den insamlade informationen på det sätt som behövs i ditt program.
```
Go
file, err := os.Open("textfil.txt")
if err != nil {
  log.Fatal(err)
}
scanner := bufio.NewScanner(file)
for scanner.Scan() {
  fmt.Println(scanner.Text())
}
```
Detta kodexempel öppnar textfilen "textfil.txt" och skriver ut varje rad med hjälp av fmt.Println (). Du kan också använda en "scanner.Bytes ()" funktion för att samla in informationen som en byte-slice istället för en sträng.

## Djupdykning:
Läsning av textfiler i datorspråk går långt tillbaka i historien och är fortfarande en viktig del av programmering idag. Det finns olika metoder och paket i andra programmeringsspråk som utför samma uppgift, men Go's inbyggda metoder för läsning av filer gör det enkelt och effektivt att utföra denna uppgift.

## Se även:
För mer information om läsning av textfiler i Go, se dokumentationen om "os" och "bufio" paketen. Du kan också utforska andra sätt att hantera filer med "io" paketet.