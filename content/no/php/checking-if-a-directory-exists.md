---
title:                "Sjekke om en mappe eksisterer"
date:                  2024-01-20T14:58:00.022112-07:00
html_title:           "Fish Shell: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sjekke om en katalog eksisterer dreier seg om å bekrefte at en filsti leder til en faktisk mappe. Programmerere gjør dette for å unngå feil ved filoperasjoner og for å sikre at deres skript oppfører seg som forventet.

## Hvordan:
```php
<?php
$directory = "/min/katalog/sti";

if (is_dir($directory)) {
    echo "Katalogen eksisterer!";
} else {
    echo "Katalogen finnes ikke.";
}
?>
```
Resultat hvis katalogen eksisterer:
```
Katalogen eksisterer!
```
Resultat hvis katalogen ikke eksisterer:
```
Katalogen finnes ikke.
```

## Dypdykk:
Historisk sett har `is_dir` fungksjonen vært metoden å bruke i PHP for å sjekke om en mappe eksisterer. Et alternativ er å bruke `file_exists`, som også kontrollerer om filer eksisterer, men ikke selvstendig om det er en mappe. I praksis, hvis du bare trenger å vite om en mappe eksisterer, er `is_dir` veien å gå fordi den sikrer at stien faktisk er en katalog.

Når du jobber med filsystemer, er det også viktig å vurdere rettighetene. En PHP-script kan feile på `is_dir` om den ikke har tilstrekkelige rettigheter til å lese katalogen, selv om katalogen faktisk eksisterer. Dette må man ta høyde for og håndtere i scriptet.

Det er verdt å nevne at `is_dir` opererer på serverens filsystem når PHP-kode kjøres på en webserver. Ved lokal utvikling, sørg for at filstiene som brukes er korrekte for systemet PHP-koden kjøres på.

## Se Også:
- PHP Manual on `is_dir`: [php.net/manual/en/function.is-dir.php](https://www.php.net/manual/en/function.is-dir.php)
- PHP Manual on `file_exists`: [php.net/manual/en/function.file-exists.php](https://www.php.net/manual/en/function.file-exists.php)
- Stack Overflow for diskusjoner om filoperasjoner i PHP: [stackoverflow.com/questions/tagged/php+filesystem](https://stackoverflow.com/questions/tagged/php+filesystem)
