---
title:                "PHP: Kontrollera om en mapp existerar"
simple_title:         "Kontrollera om en mapp existerar"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp existerar är en viktig del av PHP-programmering eftersom det kan hjälpa till att undvika fel i koden genom att säkerställa att rätt filer och mappar finns innan de behövs.

## Hur man gör

För att kontrollera om en mapp existerar i PHP, används funktionen `file_exists()`. Här är ett enkelt exempel:

```PHP
<?php
// Ange sökvägen till mappen som ska kontrolleras
$mapp = "bilder/";

// Kontrollera om mappen existerar
if (file_exists($mapp)) {
    echo "Mappen finns!";
} else {
    echo "Mappen finns inte!";
}
?>
```

Om mappen "bilder" finns kommer detta att ge följande utmatning:

`Mappen finns!`

Vid behov kan du också ange en sökväg till en arbetskatalog som en andra parameter i `file_exists()`-funktionen.

## Djupdykning

När du kontrollerar om en mapp existerar kan det vara bra att veta att funktionen också fungerar för att kontrollera om en fil eller en extern webbadress existerar. Om du behöver specificera sökvägen till en extern webbadress, kom ihåg att ange ett prefix såsom "http://" eller "https://" för att funktionen ska fungera korrekt.

Det är också värt att notera att `file_exists()`-funktionen inte kan avgöra om du har tillgång till en mapp eller fil. Den endast kontrollerar om den finns eller inte. Om du också behöver kontrollera åtkomstbehörigheter, kan du använda funktionen `is_readable()` för att kontrollera om en fil eller mapp är läsbar.

## Se också

- En djupare förståelse för olika filsystemsfunktioner i PHP: [PHP Filesystem Functions (PHP Filsystemsfunktioner)](https://www.php.net/manual/en/ref.filesystem.php)
- Hur man hanterar användarens filuppladdningar: [Handling File Uploads in PHP (Hantera filuppladdningar i PHP)](https://www.php.net/manual/en/features.file-upload.php) 
- Att använda PHPs inbyggda funktioner i säkerhetssyfte: [Common Security Pitfalls with File Uploads (Vanliga säkerhetsfallgropar med filuppladdningar)](https://www.owasp.org/index.php/Unrestricted_File_Upload)