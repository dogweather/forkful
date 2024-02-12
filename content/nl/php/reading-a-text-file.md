---
title:                "Een tekstbestand lezen"
aliases:
- nl/php/reading-a-text-file.md
date:                  2024-01-28T22:05:25.231198-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tekstbestand lezen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/php/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een tekstbestand lezen in PHP betekent inhoud uit een bestand halen en in je script brengen. Programmeurs doen dit om gegevensopslag, configuratie te beheren, of om grote datasets te verwerken zonder hun code te belasten.

## Hoe te:
### Gebruikmakend van `file_get_contents`:
```PHP
$content = file_get_contents("example.txt");
echo $content;
```
Voorbeelduitvoer:
```
Hallo, Wereld!
Dit is inhoud uit het tekstbestand.
```

### Gebruikmakend van `fopen` en `fgets`:
```PHP
$handle = fopen("example.txt", "r");
if ($handle) {
    while (($line = fgets($handle)) !== false) {
        echo $line;
    }
    fclose($handle);
}
```
Voorbeelduitvoer:
```
Hallo, Wereld!
Dit is inhoud uit het tekstbestand.
```

### Schrijven naar een bestand met `file_put_contents`:
```PHP
$newContent = "Nieuwe tekst toevoegen.";
file_put_contents("example.txt", $newContent);
```

## Diepere Duik
Tekstbestanden lezen is zo oud als programmeren zelf. Voordat databases er waren, bevonden configuratiebestanden en gebruikersgegevens zich vaak in eenvoudige tekstbestanden. Alternatieven zoals XML- en JSON-bestanden zijn gestructureerd, makkelijker te parsen en geschikt voor complexe gegevens.

In PHP zijn `file_get_contents` en `file()` snel voor het lezen; de eerste haalt alles in één string op, en de laatste in een array. `fopen` in combinatie met `fgets` of `fread` geeft je meer controle, vooral voor grote bestanden, omdat je het regel voor regel of in stukken leest.

Enkele nuances: `fopen` vereist de juiste permissies, anders faalt het; het afhandelen van zijn fouten is een best practice. Wees je er bij het gebruik van `file_put_contents` van bewust dat het standaard het bestand overschrijft; gebruik de `FILE_APPEND` vlag om in plaats daarvan inhoud toe te voegen.

## Zie Ook
- PHP Manual over `file_get_contents`: https://www.php.net/manual/en/function.file-get-contents.php
- PHP Manual over `fopen`: https://www.php.net/manual/en/function.fopen.php
- PHP Manual over `fgets`: https://www.php.net/manual/en/function.fgets.php
- PHP Manual over `file_put_contents`: https://www.php.net/manual/en/function.file-put-contents.php
- Tutorial over PHP-bestandsafhandeling: https://www.w3schools.com/php/php_file.asp
