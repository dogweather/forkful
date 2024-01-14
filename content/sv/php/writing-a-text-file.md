---
title:                "PHP: Att skriva en textfil"
simple_title:         "Att skriva en textfil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Varför

Att kunna skriva en textfil är en grundläggande färdighet för varje PHP-programmerare. Det tillåter oss att lagra data permanent på en server och använda det i våra program.

# Hur man gör det

För att skriva en textfil i PHP, behöver vi först öppna en fil med fopen() funktionen. Det första argumentet är filens namn, och det andra argumentet anger åtkomstläge, som kan vara "r" för läsning, "w" för skrivning eller "a" för append. För att skriva in i filen använder vi fwrite() funktionen och stänger sedan filen med fclose().

```PHP
$fil = fopen("minfil.txt", "w"); // öppna filen för skrivning
fwrite($fil, "Detta är en text som jag skriver i filen."); // skriv in texten
fclose($fil); // stäng filen
```

Om filen redan finns kommer fopen() att öppna den för skrivning och skriva över all tidigare data. Om vi vill lägga till ny data utan att skriva över den befintliga datan, bör vi använda "a" som åtkomstläge istället.

# Djupdyk

Att skriva en textfil är bara en del av processen för att spara data i en fil. Det finns också sätt att läsa från en textfil, uppdatera befintlig data och hantera eventuella fel som kan uppstå. För en mer detaljerad förklaring och fler exempel, besök gärna PHP:s dokumentation för filhantering.

# Se även

[PHP:s dokumentation för filhantering](https://www.php.net/manual/en/ref.filesystem.php)