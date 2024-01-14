---
title:                "Ruby: Läsning av en textfil"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil är en grundläggande färdighet i programmering, oavsett vilket språk du använder. Det är viktigt att kunna läsa och manipulera filer för att kunna skapa effektiva program. Därför är det viktigt att ha goda kunskaper i att läsa en textfil med Ruby.

## Så här gör du

För att läsa en textfil med Ruby använder vi metoden `File.open`, som tar två argument - filnamn och läge. Om vi vill öppna en fil som befinner sig i samma mapp som vårt ruby-program, kan vi använda `File.open("filnamn.txt", "r")`. Detta öppnar filen i läget "r" som betyder read, vilket innebär att vi endast kan läsa filen.

För att faktiskt läsa filen använder vi metoden `File.read`, som tar det öppnade filobjektet som argument. Detta returnerar innehållet i filen som en sträng, som vi sedan kan lagra i en variabel eller använda direkt i vårt program.

```Ruby
fil = File.open("textfil.txt", "r")

innehall = File.read(fil)

puts innehall
```

Detta kodblock öppnar filen "textfil.txt", läser innehållet och skriver ut det i terminalen.

## Djupdykning

Att läsa en textfil innebär mer än bara att använda `File.open` och `File.read`. Det finns flera andra metoder som kan användas för att läsa och manipulera filer, som till exempel `File.readlines`, som läser in varje rad i filen som ett element i en array, eller `File.each_byte`, som läser in varje enskild byte i filen.

Det finns också möjlighet att läsa in textfiler som CSV-filer, där varje rad representeras som en array med innehållet i kolumnerna. Detta är en vanlig metod för att läsa in stora mängder data från filer.

Det är också viktigt att ha kunskap om filströmmar och hur man hanterar dem korrekt för att inte orsaka minnesläckor eller andra problem när man läser filer i stora mängder.

## Se även

- [Ruby-dokumentationen för File-klassen](https://ruby-doc.org/core-3.0.1/File.html)
- [Kodexempel för att läsa en textfil i Ruby](https://www.sitepoint.com/reading-files-in-ruby/)
- [Lär dig mer om filströmmar i Ruby](https://www.cloudbees.com/blog/file-operations-ruby-file-operations-and-file-commands)