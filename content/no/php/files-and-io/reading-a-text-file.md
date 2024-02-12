---
title:                "Lese en tekstfil"
aliases:
- /no/php/reading-a-text-file/
date:                  2024-01-20T17:54:47.012585-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese en tekstfil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å lese en tekstfil i PHP betyr å hente data fra en fil på serveren. Programmerere gjør dette for å behandle informasjon, lagre innstillinger, eller å vise innhold til brukere.

## Hvordan:
Lese en fil linje for linje med `fgets()`:

```PHP
<?php
$fil = fopen("min_fil.txt", "r") or die("Kan ikke åpne filen!");

while (!feof($fil)) {
    $linje = fgets($fil);
    echo $linje;
}

fclose($fil);
?>
```

Lese hele filen på en gang med `file_get_contents()`:

```PHP
<?php
$innhold = file_get_contents("min_fil.txt");
echo $innhold;
?>
```

Output vil være innholdet i 'min_fil.txt' vist i nettleseren.

## Dypdykk
Å lese filer i PHP har røtter helt tilbake til de tidlige dagene av språket. Alternativer til `fgets()` og `file_get_contents()` omfatter `file()` som leser en fil til et array, og `fread()` for å lese en viss mengde bytes.

Når det gjelder implementering, håndterer PHP forskjellige filformater og kodek. Det er viktig å huske på filtilganger og rettigheter for å unngå sikkerhetsrisikoer ved lesing av filer.

## Se Også
- PHPs offisielle dokumentasjon for filhåndtering: https://www.php.net/manual/en/book.filesystem.php
- Sikkerhetsconsiderations ved file read operations: https://www.php.net/manual/en/security.filesystem.php
- En dypere utforskning av datatyper og filsystem funksjoner: https://www.php.net/manual/en/ref.filesystem.php
