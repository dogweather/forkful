---
title:                "PHP: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Varför

I denna blogginlägg kommer vi att utforska hur man kan läsa innehållet från en textfil in i en PHP-applikation. Att läsa en textfil kan vara användbart för att hämta data från en extern källa, till exempel en databas eller en API. Det kan också vara ett bra sätt att lagra och hantera information som inte behöver vara interaktiv eller dynamisk.

# Hur man gör

För att läsa en textfil i PHP behöver vi först öppna filen med hjälp av funktionen `fopen()`. Sedan kan vi använda en loop för att läsa filen rad för rad med hjälp av funktionen `fgets()`. Till sist är det viktigt att stänga filen när vi är klara med hjälp av funktionen `fclose()`.

```PHP
$file = fopen("textfil.txt", "r"); // Öppnar filen i läsläge

// Loopar igenom filen och läser varje rad
while(!feof($file)) {
  $line = fgets($file);
  echo $line;
}

fclose($file); // Stänger filen
```

Om vi till exempel har en textfil med namnen på olika länder på varje rad, så kommer utskriften att bli:

```
Sverige
USA
Kina
Japan
```

# Djupdykning

Det finns flera olika sätt att läsa en textfil i PHP, beroende på den specifika användningen och behoven. Utöver `fgets()` finns det även andra funktioner som `fread()` och `file()` som kan vara användbara i olika situationer. Vi kan också använda olika flaggor när vi öppnar filen med `fopen()` för att till exempel ange vilken läs- och skrivmetod som ska användas.

Det är också viktigt att tänka på att filen måste ha rätt rättigheter för att kunna läsas av PHP. Om du stöter på problem när du läser en textfil, så är det värt att kontrollera att filen har de rättigheter som krävs för att läsas.

# Se även

- [PHP Manual: Filesystem Functions](https://www.php.net/manual/en/ref.filesystem.php)
- [The Ultimate Guide to Reading Files in PHP](https://code.tutsplus.com/tutorials/the-ultimate-guide-to-reading-files-in-php--cms-21383)
- [PHP File Handling](https://www.w3schools.com/php/php_file.asp)