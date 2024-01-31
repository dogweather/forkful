---
title:                "Skriving av en tekstfil"
date:                  2024-01-19
simple_title:         "Skriving av en tekstfil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skriving til en tekstfil lar deg lagre data persistent. Det er grunnleggende for logging, lagring av konfigurasjoner, og datadeling mellom skript eller sesjoner.

## Hvordan gjøre det:
```PHP
<?php
$filnavn = "eksempel.txt";
$innhold = "Hei Verden!\n";

// Åpner filen for skriving, lager filen hvis den ikke finnes.
$fhandle = fopen($filnavn, 'w');

// Skriver innhold til filen.
fwrite($fhandle, $innhold);

// Lukker filhåndtereren.
fclose($fhandle);

echo "Fil skrevet: " . filesize($filnavn) . " bytes";
?>
```
Output:
```
Fil skrevet: 12 bytes
```

## Dypdykk
Før PHP 4.x, brukere stolte mer på system-spesifikke funksjoner for filhåndtering. Alternativer inkluderer file_put_contents() for enklere syntaks, eller strømningsvise operasjoner for store filer. Viktige implementasjonsdetaljer inkluderer filrettigheter, riktig håndtering av ressurser, og unntakssikkerhet.

## Se også
- PHP-dokumentasjon på filsystemfunksjoner: https://www.php.net/manual/en/ref.filesystem.php
- file_put_contents-dokumentasjon: https://www.php.net/manual/en/function.file-put-contents.php
- Offisielle PHP læreressurser: https://www.php.net/manual/en/faq.php.developers
