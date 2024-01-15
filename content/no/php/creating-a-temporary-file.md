---
title:                "Opprettelse av en midlertidig fil"
html_title:           "PHP: Opprettelse av en midlertidig fil"
simple_title:         "Opprettelse av en midlertidig fil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##

Hvorfor:

Skal du til å programmere og lurer på hvorfor du skulle bry deg med å lage midlertidige filer? Det er flere situasjoner der det kan være nyttig å bruke midlertidige filer i din PHP-kode. De kan for eksempel brukes til å lagre midlertidige data som trengs for å kjøre en bestemt funksjon, eller til å utføre operasjoner på større filer som krever et midlertidig lagringssted.

## Hvordan:


Hvis du ønsker å lage en midlertidig fil i PHP, kan du bruke funksjonen `tmpfile()`. Denne funksjonen oppretter en midlertidig fil og returnerer en håndterer som kan brukes til å utføre operasjoner på filen. Se følgende kode eksempel:

```PHP
<?php
$file = tmpfile();
echo gettype($file). "\n"; // output: resource
?>
```

Som du kan se, returnerer `tmpfile()` en håndterer av typen "resource". Du kan deretter bruke denne håndtereren til å skrive til filen, lese fra den, eller utføre andre manipulasjoner på den. Husk å lukke filen etter at du er ferdig med å bruke den ved å bruke funksjonen `fclose()`.

```PHP
<?php
$file = tmpfile();
fwrite($file, "Dette er en midlertidig fil\n");
rewind($file); // Setter håndtereren tilbake til starten av filen
echo fgets($file); // output: Dette er en midlertidig fil
fclose($file); // Husk å lukke filen når du er ferdig
?>
```

## Deep Dive:

Når du oppretter en midlertidig fil ved hjelp av `tmpfile()` blir filen automatisk slettet når den er lukket. Dette betyr at du ikke trenger å bekymre deg for å slette filen manuelt etter at du er ferdig med å bruke den. Du kan også bruke funksjonen `tmpname()` som gir deg navnet på den midlertidige filen, og dermed kan du eksempelvis inkludere den i en kobling for å sende data videre til en annen side.

En annen nyttig funksjon er `unlink()` som lar deg slette en fil på serveren. Dette kan være nyttig om du ønsker å slette den midlertidige filen før du lukker den, ved å kalle `unlink()` før du bruker `fclose()`.

## Se Også:

- [PHP.net - tmpfile()](https://www.php.net/manual/en/function.tmpfile.php)
- [PHP.net - tmpnam()](https://www.php.net/manual/en/function.tmpnam.php)
- [PHP.net - unlink()](https://www.php.net/manual/en/function.unlink.php)