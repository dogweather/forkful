---
title:                "Skriva en textfil"
html_title:           "Elm: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

##Varför
Att skriva ett textdokument är ett viktigt element i programmering. Det bidrar till att organisera och strukturera din kod samt göra det lättare att förstå och förklara för andra.

##Hur man gör det 
Här är ett exempel på hur du kan skapa en textfil i Elm:

```Elm
-- Skapa en textfil med namnet "mittDokument.txt"
textFile = "mittDokument.txt"

-- Öppna filen för skrivning
file = File.Open textFile (File.Writeable True)

-- Skriv till filen
File.write file "Det här är en textfil som skapats med hjälp av Elm."

-- Stäng filen
File.close file
```

När du kör detta kodblock, kommer en ny textfil med namnet "mittDokument.txt" att skapas i samma mapp som ditt Elm-program. Om du öppnar filen, kommer du att se att texten "Det här är en textfil som skapats med hjälp av Elm." har lagts till.

##Djupdykning
När du skapar en textfil i Elm, finns det några saker att tänka på:

1. Varje rad i filen måste avslutas med en radbrytning, annars kommer all text att visas på en enda rad när du öppnar filen.
2. Du kan också använda `File.append` för att lägga till text till en befintlig textfil istället för att skriva över den.
3. Se till att inkludera rätt behörigheter när du öppnar filen för läsning eller skrivning. Om du vill tillåta andra att läsa eller skriva i filen kan du ange `File.readable True` respektive `File.writeable True`.

##Se även
- Elm dokumentation för filhanteringsfunktioner: https://package.elm-lang.org/packages/elm/file/latest/
- En tutorial om hur man skapar en textfil i Elm: https://dev.to/prikhi/creating-and-modifying-files-in-elm-47f