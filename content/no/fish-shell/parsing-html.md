---
title:                "Fish Shell: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor
Å tolke HTML kan være en svært nyttig ferdighet for Fish Shell-programmerere. Det gjør det mulig å analysere og manipulere nettsideinnhold, som kan være nyttig for automatisering eller web scraping.

## Slik gjør du det
Det første du trenger å gjøre er å installere `curl`-pakken ved å kjøre følgende kommando i terminalen:

```Fish Shell
$ pakkeinstall curl
```

Deretter kan du bruke `curl` for å hente HTML fra en nettside og lagre det i en variabel ved å bruke `set`-kommandoen:

```Fish Shell
$ set html_curl (curl -s https://www.example.com)
```

Nå kan du bruke `string`-kommandoen til å utføre forskjellige operasjoner på HTML-innholdet. For eksempel, hvis du bare ønsker å se på innholdet av en bestemt HTML-tag, kan du bruke følgende kommando:

```Fish Shell
$ string extract -r '<tag>.*<\/tag>' $html_curl
```

Dette vil returnere alle forekomster av `<tag>` og deres innhold i HTML-en.

## Mer informasjon
Hvis du ønsker å lære mer om hvordan du kan tolke HTML med Fish Shell, kan du utforske noen av de forskjellige plugins og pakker som er tilgjengelige. For eksempel, `htmlparse`-pluginet kan være nyttig for å tolke HTML ved hjelp av CSS-selektorer.

## Se også
- [Fish Shell nettside](https://fishshell.com/)
- [Offisiell dokumentasjon for Fish Shell](https://fishshell.com/docs/current/index.html)
- [Curl pakke for Fish Shell](https://fishshell.com/docs/current/commands.html#curl)
- [Htmlparse plugin for Fish Shell](https://github.com/fish pkg/htmlparse)