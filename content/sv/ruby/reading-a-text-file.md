---
title:                "Läsa en textfil"
html_title:           "Ruby: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Det finns många olika användningsområden för att läsa en textfil i Ruby. Det kan vara för att hämta och manipulera data från en annan källa, som att skapa en användarlista från en CSV-fil eller generera en rapport baserad på informationen i filen. Det är också ett vanligt steg i många programmeringsprojekt, oavsett om det är för att läsa konfigurationsfiler eller behandla användarinmatning.

## Hur man gör

För att läsa en textfil i Ruby, behöver du först öppna filen och sedan loopa igenom varje rad. Det finns olika sätt att öppna en fil beroende på vilken typ av åtkomst du behöver, men den vanligaste är att använda File-klassen.

```Ruby
File.open("textfil.txt", "r") do |file|
    file.each_line do |line|
        puts line
    end
end

# Output: 
# This is the first line.
# This is the second line.
# This is the third line.
```

I exemplet ovan öppnar vi filen "textfil.txt" i läs-mode och sedan loopar igenom varje rad med hjälp av `each_line`-metoden. Vi använder också `puts` för att skriva ut varje rad på skärmen.

Om du vill läsa en fil rad för rad utan att använda en `do`-loop, kan du också använda `readlines`-metoden.

```Ruby
file = File.open("textfil.txt", "r")
lines = file.readlines
file.close

puts lines

# Output:
# ["This is the first line.", "This is the second line.", "This is the third line."]
```

Här öppnar vi filen, läser varje rad med `readlines` och sparar dem i en array. Vi stänger också filen efter att vi är färdiga med att läsa den.

För att få tillgång till en specifik rad eller ett specifikt stycke av en fil, kan vi använda metoden `read` tillsammans med en radnummer.

```Ruby
file = File.open("textfil.txt", "r")
line = file.read(2)
puts line

# Output:
# Th
```

Här öppnar vi filen och använder `read` för att bara läsa de första två tecknen från första raden i filen. Det här är användbart när du har stora filer och bara behöver hämta viss information.

## Djupdykning

Vid läsning av en textfil i Ruby finns det fler metoder som kan vara användbara beroende på dina behov. Till exempel kan du använda `readchar` för att läsa en karaktär i taget, `readbyte` för att läsa en byte i taget eller `rewind` för att återgå till början av filen efter att du har loopat igenom den.

Du kan också använda olika åtkomstlägen när du öppnar en fil, som läs- /skriv-, append- eller skriv-läge. Det kan också vara viktigt att komma ihåg att stänga filen när du är klar med den, annars kan det leda till problem senare i ditt program.

## Se också

- [File-klassen](https://ruby-doc.org/core-2.7.0/File.html)
- [Öppna en fil i Ruby](https://medium.com/@juandebravo94/opening-files-in-ruby-fbd3c805cba1)
- [Flera sätt att läsa textfiler i Ruby](https://stackify.com/how-to-read-a-file-in-ruby/)