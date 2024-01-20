---
title:                "Läsa en textfil"
html_title:           "Fish Shell: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att läsa en textfil innebär att använda programmering för att hämta information från en extern fil. Programmerare gör det för att behandla, analysera eller ändra fildatat utan att manuellt öppna och ändra filen.

## Så här gör du:

PHP levereras med flera praktiska funktioner för att läsa textfiler. Dessa är `file()`, `file_get_contents()`, och `fopen()`. Här är en snabb startguide om hur du använder `file_get_contents()`.

```PHP
<?php

$filename = "testfile.txt";

$fileContents = file_get_contents($filename);

echo $fileContents;
?>
```

Om "testfile.txt" innehåller texten "Hej Världen!", då kommer outputen vara just det.

## Deep Dive

Historiskt sett har det alltid varit nödvändigt för programmerare att läsa och skriva till filer, även från dagarna av forntalet maskinkod. PHP, trots sin uppstart som ett enkelt scriptspråk för att skapa dynamiska webbsidor, erbjuder robusta filhanteringsfunktioner.

Alternativen till `file_get_contents()` är `fopen()`, som är mer lämplig för stora filer, eftersom den läser filen i ström och inte allt på en gång som `file_get_contents()`. `file()` är ett annat alternativ som läser filen i en array, vilket kan vara praktiskt om du behöver manipulera individuella linjer.

Implementationen av att läsa textfiler i PHP är ganska enkel, men det finns några filöppningsinställningar att tänka på. Att läsa en fil kräver läsbehörigheter, så se till att din PHP-server har korrekta behörigheter för filen du vill läsa.

## Se Även

Här är några extra resurser:

- PHP.net dokumentation på Filesystem Functions: https://www.php.net/manual/en/ref.filesystem.php
- W3Schools: PHP File Open/Read/Close: https://www.w3schools.com/php/php_file_open.asp
- Stack Overflow Discussion på `file_get_contents()` vs `fopen()`: https://stackoverflow.com/questions/555523/file-get-contents-vs-fopen-fread