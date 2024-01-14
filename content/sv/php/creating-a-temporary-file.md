---
title:                "PHP: Skapa en temporär fil"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Att skapa en temporär fil kan vara användbart i många olika situationer inom PHP-programmering. Det kan till exempel vara användbart när du vill ha en tillfällig plats att lagra data under en session eller när du behöver hämta en extern fil för behandling.

## Så här gör du

Här är ett enkelt exempel på hur du kan skapa en temporär fil i PHP:

```PHP
// Skapa en ny temporär fil
$tempFile = tmpfile();

// Skriv till filen
fwrite($tempFile, "Det här är ett exempel på innehållet i den temporära filen.");

// Visa sökvägen till den temporära filen
echo "Sökvägen till den temporära filen är: " . stream_get_meta_data($tempFile)['uri'];

// Stäng filen
fclose($tempFile);
```

När du kör detta kodexempel bör du få ut en sökväg till den temporära filen, till exempel: "/tmp/phpBp1FVU". Det är viktigt att komma ihåg att den temporära filen automatiskt kommer att tas bort när scriptet avslutas eller när filen stängs. Om du vill behålla den temporära filen under en längre tid måste du använda `sys_get_temp_dir()`-funktionen för att få sökvägen till det temporära mappen och därefter ändra filnamnet till något unikt.

## På djupet

När du använder `tmpfile()`-funktionen i PHP för att skapa en temporär fil, kommer filen att skapas i den temporära katalogen på servern. Detta kan variera beroende på vilket operativsystem och vilken server du använder. Om du vill ha mer kontroll över var filen ska skapas kan du istället använda `tmpnam()`-funktionen och ange en önskad sökväg.

Det är också viktigt att komma ihåg att storleken på en temporär fil är begränsad av det tillgängliga utrymmet på servern. Om du behöver skapa en temporär fil som är större än detta, kan du använda `ftruncate()`-funktionen för att öka filens storlek.

## Se även

- [PHP: tmpfile() function](https://www.php.net/manual/en/function.tmpfile.php)
- [PHP: tmpnam() function](https://www.php.net/manual/en/function.tmpnam.php)
- [PHP: ftruncate() function](https://www.php.net/manual/en/function.ftruncate.php)