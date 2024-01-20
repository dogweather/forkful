---
title:                "Skriva en textfil"
html_title:           "PHP: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att skriva en textfil är en vanlig uppgift för programmerare. Det innebär att skriva en fil som innehåller text, till exempel en lista, ett meddelande eller en databas. Programmerare gör detta för att lagra och hantera data på ett enkelt och strukturerat sätt.

## Så här gör du:
För att skapa en textfil används PHPs inbyggda funktion "fopen()". Först måste du ange ett namn och en sökväg för filen du vill skapa. Sedan kan du använda funktioner som "fwrite()" eller "file_put_contents()" för att skriva din text till filen. Här är ett exempel på hur detta kan se ut:

```PHP
$filename = "min-textfil.txt"; // Du kan byta ut filnamnet och sökvägen för att anpassa det efter ditt projekt
$file = fopen($filename, "w+"); // "w+" betyder att vi vill skriva till filen
$text = "Det här är min första textfil!";

// Skriver texten till filen
fwrite($file, $text);

// Stänger filen
fclose($file);

// Resultatet blir att en fil med namnet "min-textfil.txt" skapas och innehåller texten "Det här är min första textfil!"
```

## Nerdy stuff
Att skapa och hantera textfiler är en grundläggande del av programmering och har funnits sedan de tidiga dagarna av datorer. Det finns också alternativ till fopen() som till exempel "file_put_contents()" som möjliggör att du kan skriva till en fil utan att först behöva öppna den. Du kan också använda filsystemet "Linux" för att hantera filer på ditt server och sedan skriva till dem med PHP-kod.

## Relaterade källor
Här är länkar till andra källor med mer information om att skriva textfiler i PHP:
- [W3Schools - PHP File Handling](https://www.w3schools.com/php/php_file.asp)
- [PHP Manual - fopen()](https://www.php.net/manual/en/function.fopen.php)