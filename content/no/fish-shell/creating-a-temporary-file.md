---
title:                "Fish Shell: Oppretting av en midlertidig fil"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lage midlertidige filer er en vanlig og nyttig operasjon i utviklingen av programmering. De kan brukes til å midlertidig lagre data, lage backup-filer, eller brukes som en midlertidig buffer for kommunikasjon mellom forskjellige deler av en applikasjon. Uansett hva behovet ditt er, er det en enkel og effektiv måte å lage midlertidige filer i Fish Shell.

## Hvordan lage en midlertidig fil

Det enkleste måten å lage en midlertidig fil i Fish Shell er å bruke `mktemp` kommandoen. Dette kommandoen vil automatisk opprette en midlertidig fil i det eksisterende katalogen. Her er et eksempel på hvordan man kan lage en midlertidig fil og skrive litt data inn i den:

```Fish Shell
set my_file (mktemp)
echo "Dette er en midlertidig fil" > $my_file
```

La oss nå se på innholdet i den nye midlertidige filen ved å bruke `cat` kommandoen:

```Fish Shell
cat $my_file
```

Her er den forventede utgangen:

```
Dette er en midlertidig fil
```

Som du kan se, ble den midlertidige filen opprettet og data ble skrevet inn i den. Når du er ferdig med å bruke den midlertidige filen, kan du slette den ved å bruke `rm` kommandoen. Husk å bruke `$my_file` variabelen når du sletter filen.

```Fish Shell
rm $my_file
```

## Dypdykk

For de som ønsker en dypere forståelse av hvordan midlertidige filer fungerer, kan vi se på noen av de interne funksjonene i Fish Shell som brukes til å opprette og administrere disse filene.

En midlertidig fil er opprettet ved å bruke `mktemp` kommandoen og denne kommandoen bruker faktisk en intern funksjon kalt `_fish_mktemp` for å generere filnavnet. Denne funksjonen bruker utgangen fra `mktemp -q` kommandoen som et grunnlag for å generere et unikt filnavn. Filnavnet inneholder også brukernavn og prosessidentifikasjon (PID) for å sikre at det er unikt for hver bruker og prosess.

Når filen er opprettet og brukes til å lagre data, blir innholdet i den vanligvis kopiert til en midlertidig buffer i minnet. Dette gjør at programmet kan oppdatere dataene uten å skrive tilbake til den fysiske filen, noe som forbedrer hastigheten på programmet.

## Se også

- http://fishshell.com/docs/current/cmds/mktemp.html
- http://fishshell.com/docs/current/index.html#variables
- http://fishshell.com/docs/current/scripting.html#internal-functions