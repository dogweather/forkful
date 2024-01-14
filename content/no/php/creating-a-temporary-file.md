---
title:                "PHP: Lage en midlertidig fil"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger, når du programmerer med PHP, trenger du midlertidige filer for å lagre data mens applikasjonen kjører. Dette kan være for å redusere lagringsbehovet eller for å lagre informasjon som kun trengs i øyeblikket. Å opprette midlertidige filer kan være nyttig i en rekke situasjoner, og i denne bloggposten vil vi utforske hvorfor og hvordan du kan gjøre det.

## Hvordan du oppretter midlertidige filer i PHP

For å opprette en midlertidig fil i PHP, kan du bruke funksjonen `tempnam()`. Denne funksjonen tar inn to parametere, en mappebane hvor den midlertidige filen skal opprettes og et valgfritt prefiks for filnavnet. For eksempel:

```PHP
$temp_file = tempnam('/tmp', 'my_prefix');
```

Denne koden vil opprette en tekstfil under mappen `/tmp` med et tilfeldig generert navn, men med prefikset `my_prefix` foran. Dette gir deg en unik fil å jobbe med.

Når du er ferdig med å bruke den midlertidige filen, må du slette den for å frigjøre plass på serveren. For dette kan du bruke funksjonen `unlink()`:

```PHP
unlink($temp_file);
```

## Dypdykk

Når du oppretter en midlertidig fil, blir den lagret på serveren i en mappe som er tilgjengelig for webserveren. Det er viktig å sørge for at denne mappen er riktig konfigurert og beskyttet mot uautorisert tilgang. En annen forholdsregel du bør ta, er å ikke lagre sensitiv informasjon i den midlertidige filen, da den kan være sårbar for lekkasje av data.

En annen ting å merke seg er at midlertidige filer vil bli slettet automatisk når skriptet er ferdig å kjøre. Men det er alltid en god praksis å slette filen manuelt for å sikre at den ikke forblir på serveren.

## Se også

- [PHP-tutorials: Opprett og slett midlertidige filer](https://www.php.net/manual/no/function.tempnam.php)
- [Secure Programming Techniques for PHP](https://www.php.net/manual/no/book.security.php)