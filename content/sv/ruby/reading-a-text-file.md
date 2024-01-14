---
title:    "Ruby: Läsa en textfil"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Varför
Att läsa en textfil är viktigt inom programmering eftersom det låter dig få tillgång till och hantera stora mängder data. Detta kan vara användbart i många olika programmeringsprojekt.

## Hur man gör det
Läsning av en textfil i Ruby är en relativt enkel process. Först måste du öppna filen med hjälp av File klassen och sedan kan du börja läsa igenom den. Här är ett exempel på hur en kod skulle kunna se ut:

```Ruby
file = File.open("textfil.txt", "r") #öppnar filen i läsläge
file.each_line do |line| #läser varje rad i filen
  puts line #skriver ut raden till terminalen
end
file.close #stänger filen när du är klar med att läsa
```

Om vi antar att vår textfil har följande innehåll:

```
Hej
Jag heter Ruby
Detta är en textfil
```

Då kommer utmatningen från exemplet att vara:

```
Hej
Jag heter Ruby
Detta är en textfil
```

## Djupdykning
När vi läser en textfil i Ruby, går vi igenom varje rad i filen. Men det finns även andra sätt att hantera filen, såsom att läsa in hela filen till en variabel för vidare bearbetning eller att använda specifika metoder för att söka igenom filen. Det finns också möjlighet att skriva till en textfil på liknande sätt som vi läser den. Detaljerade instruktioner för detta finns tillgängliga på Rubys dokumentationssida.

## Se även
Rubys dokumentation för filoperationer: https://ruby-doc.org/core-2.7.2/File.html
En guide om att läsa och skriva till filer i Ruby: https://www.digitalocean.com/community/tutorials/how-to-read-and-write-to-files-in-ruby-sv