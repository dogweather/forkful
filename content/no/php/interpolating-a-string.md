---
title:                "Interpolering av en streng"
html_title:           "Bash: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Setter Sammen Strenger med PHP: En Introduksjon til Interpolering

## Hva & Hvorfor?
Interpolering av strenger i PHP er prosessen med å sette inn variabler direkte inn i en tekststreng. Programmerere bruker det fordi det er en kortfattet og lesbar måte å kombinere variabler og tekst på.

## Hvordan:
Her er eksemplode på koding og utdata:

```PHP
$fornavn = "Ola";
$etternavn = "Nordmann";
echo "Hei, jeg heter $fornavn $etternavn.";
```

Output:

```
Hei, jeg heter Ola Nordmann.
```

Med hendig interpolering, kan du enkelt skrive ut en hel setning med variabler innebygget.

## Dypdykk
Historisk sett, hadde tidligere versjoner av PHP ikke støtte for interpolering. Dette ble lagt til som en del av makroprosesseringsfunksjonene for å gjøre det enklere å skrive kode.

Alternativt kan programmerere bruke concatenation for å oppnå lignende resultater. For eksempel:

```PHP
echo 'Hei, jeg heter ' . $fornavn . ' ' . $etternavn . '.';
```

Det er viktig å merke seg at interpolering bare fungerer med doble anførselstegn. Enkelte anførselstegn tolker ikke variabler, og vil i stedet bare vise strengen som den er.

## Se Også
For mer informasjon og relaterte ressurser, kan du besøke følgende lenker:
- PHP Manual String Syntax: [Her](https://www.php.net/manual/en/language.types.string.php#language.types.string.syntax.double)
- PHP The Right Way: [Her](https://phptherightway.com/)