---
title:                "Läsning av textfil"
html_title:           "PHP: Läsning av textfil"
simple_title:         "Läsning av textfil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför
Att läsa och hantera textfiler är en grundläggande kunskap inom programmering och är särskilt viktigt vid hantering av stora mängder data. Att läsa en textfil i PHP kan vara användbart för att hämta och behandla information från externa källor eller för att analysera loggfiler.

## Hur man gör det
### Öppna en textfil
För att öppna en textfil i PHP använder vi fopen() funktionen. Denna funktion tar två parametrar: sökvägen till filen och önskat läge. Läge kan vara "r" för läsning eller "w" för skrivning. Om filen inte kan öppnas kommer fopen() att returnera false. Om vi vill läsa filen lägger vi till "r" efter filvägen.

```
<?php
$fil = fopen("text_fil.txt", "r");
```
### Läsa en textfil
När filen är öppen använder vi en loop tillsammans med fgets() funktionen för att läsa in varje rad i textfilen tills vi når slutet av filen. Denna metod är effektiv för stora filer eftersom den inte läser in hela filen i minnet samtidigt.

```
<?php
while (!feof($fil)) {
  $rad = fgets($fil);
  // gör något med raden
}
```

### Stänga filen
För att undvika minnesläckor och för att frigöra resurser, är det viktigt att stänga filen efter att vi är klara med den. Detta görs genom att använda fclose() funktionen.

```
<?php
fclose($fil);
```

## Deep Dive
Det finns flera olika sätt att läsa och hantera textfiler i PHP. En annan metod är att använda file() funktionen som automatiskt läser in hela filen och returnerar en array med varje rad som ett element. Denna metod är mindre effektiv för stora filer men kan vara användbar för mindre filer.

```
<?php
$lines = file("text_fil.txt");

foreach ($lines as $line) {
  // gör något med raden
}
```

En annan användbar funktion är file_get_contents() som läser in hela filen som en sträng. Denna metod kan vara användbar för att hämta innehållet i en liten fil.

```
<?php
$innehall = file_get_contents("text_fil.txt");
```

## Se även
- [PHP officiell dokumentation för filhantering](https://www.php.net/manual/en/ref.filesystem.php)
- [Enkätprogrammering: Läs och manipulera textfiler i PHP](https://www.codewall.co.uk/read-and-manipulate-text-files-in-php/)
- [Läsa filer på ett felhanterat sätt i PHP](https://www.cloudways.com/blog/file-handling-in-php/)