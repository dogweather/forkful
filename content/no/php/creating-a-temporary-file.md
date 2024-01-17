---
title:                "Opprettelse av midlertidig fil"
html_title:           "PHP: Opprettelse av midlertidig fil"
simple_title:         "Opprettelse av midlertidig fil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Opprette midlertidig filer er en vanlig praksis blant programvareutviklere for å lagre midlertidig data eller resultater som trenger å bli brukt senere. Dette er spesielt nyttig når man arbeider med store datasett eller når programmer må koordinere med andre applikasjoner.

## Hvordan:
```
// Opprette en midlertidig fil med standardnavn og lokasjon
$temp_file = tempnam(sys_get_temp_dir(), "temp");

// Skrive data til den midlertidige filen
$file_handle = fopen($temp_file, "w");
fwrite($file_handle, "Dette er tekst som skal lagres i den midlertidige filen");
fclose($file_handle);

// Les data fra den midlertidige filen og skriv ut i nettleseren
echo file_get_contents($temp_file);

// Slette den midlertidige filen
unlink($temp_file);
```
Resultat:
```
Dette er tekst som skal lagres i den midlertidige filen
```

## Dypdykk:
Opprettelse av midlertidige filer har vært en vanlig praksis i programmering i lang tid. I eldre operativsystemer som DOS og Windows 3.x var det nødvendig å manuelt opprette midlertidige filer for å lagre data som ikke lenger var nødvendige. I dag blir midlertidige filer ofte brukt for å effektivisere plassering og utveksling av data mellom applikasjoner.

Det finnes også alternative metoder for å lagre midlertidige data i PHP, som for eksempel å bruke sesjonsvariabler eller kontinuerlig å slette gamle midlertidige filer for å frigjøre plass. Men å opprette en midlertidig fil er fortsatt en enkel og pålitelig måte å lagre og utveksle data.

Når en midlertidig fil opprettes, blir det faktisk en faktisk fil på harddisken med et unikt navn. Det er et tegn på at filen er midlertidig og bør slettes når den ikke lenger er nødvendig. Det er derfor viktig for programmere å ha kontroll over hvor midlertidige filer blir opprettet og å slette dem når de ikke lenger er i bruk.

## Se også:
https://www.php.net/manual/en/function.tempnam.php