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

## Varför
Självklart kanske du undrar varför du skulle vilja skriva en textfil med PHP. Det finns flera olika anledningar till det, såsom att spara data eller skapa en loggfil av användaraktivitet.

## Hur man gör
För att skriva en textfil med PHP behöver du först öppna en filhandtag med fopen() funktionen och ange sökvägen till filen samt vilken åtgärd som ska utföras, t.ex. läsa eller skriva. Sedan kan du använda fwrite() funktionen för att skriva in den önskade texten i filen. Avsluta sedan genom att stänga filhandtaget med fclose() funktionen.

Ett exempel på kod kan se ut så här:
```PHP
$filhandtag = fopen("nyfil.txt", "w");
fwrite($filhandtag, "Detta är en ny textfil som skrivs med PHP!");
fclose($filhandtag);
```
När filen körs kommer du se en ny textfil som heter "nyfil.txt" med innehållet "Detta är en ny textfil som skrivs med PHP!".

## Djupdykning
För att skriva en textfil med PHP kan du använda flera olika funktioner beroende på vad du vill åstadkomma. Om du vill lägga till text i en befintlig fil kan du använda funktionen file_put_contents(), medan om du vill läsa in en textfil och skriva ut den kan du använda funktionen readfile(). Det finns också möjlighet att formatera texten med hjälp av sprintf() och skriva in den i filen.

## Se också
- PHP manual för fopen() funktionen: https://www.php.net/manual/en/function.fopen.php
- Video tutorial om hur man skriver en textfil med PHP: https://www.youtube.com/watch?v=JjYs_eT7FOM
- En guide till filhantering med PHP: https://www.w3schools.com/php/php_file_open.asp