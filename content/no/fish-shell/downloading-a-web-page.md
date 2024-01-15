---
title:                "Nedlasting av en nettside"
html_title:           "Fish Shell: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor skal man engasjere seg i å laste ned en nettside? Vel, det kan være flere grunner til dette. Kanskje du ønsker å lagre en kopi av en nettside for å lese senere, eller kanskje du vil få tilgang til informasjon som ikke er tilgjengelig offline. Uansett årsak, det finnes en enkel måte å gjøre dette på ved å bruke Fish Shell.

## Hvordan
Fish Shell har en innebygd funksjon kalt `curl`, som kan brukes til å laste ned nettsider. La oss ta en titt på et eksempel:

```Fish Shell
curl example.com
```

Dette vil laste ned nettsiden `example.com` og skrive ut innholdet direkte til terminalen. Men hvis du vil lagre innholdet i en fil, kan du bruke `curl` med flagget `-o`:

```Fish Shell
curl -o example.html example.com
```

Dette vil laste ned nettsiden og lagre den som en `example.html` fil i ditt nåværende arbeidsmappe.

## Dypdykk
Nå som du vet hvordan du kan laste ned en nettside ved hjelp av Fish Shell, la oss se på noen tilleggsfunksjoner du kan bruke. Du kan for eksempel bruke `curl` med flagget `-I` for å få informasjon om headeren til en nettside:

```Fish Shell
curl -I example.com
```

Dette vil gi deg en liste over header-feltene for nettsiden, som kan være nyttig for å få informasjon om serveren, dataformatet, og mye mer.

En annen nyttig funksjon i `curl` er muligheten til å laste ned fra en spesifikk URL. Dette er veldig nyttig hvis du ønsker å laste ned en spesifikk fil fra en nettside:

```Fish Shell
curl -O example.com/images/logo.png
```

Dette vil laste ned `logo.png` filen fra `example.com` og lagre den i ditt nåværende arbeidsmappe.

## Se også
- [Offisiell Fish Shell dokumentasjon](https://fishshell.com/docs/current/)
- [Curl: Brukerveiledning](https://curl.haxx.se/docs/manpage.html)