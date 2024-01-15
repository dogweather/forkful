---
title:                "Analysering av html"
html_title:           "Fish Shell: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor

Å parse HTML kan være nyttig for å ekstrahere informasjon fra nettsider eller bygge automatiserte skript for nettlesingsoppgaver.

## Slik gjør du det

For å parse HTML i Fish Shell, kan du bruke verktøyet `lynx`, som er installert som standard på mange Linux-distribusjoner.

```Fish Shell
lynx -dump -nolist <nettside> | sed 's/.*/\x1b[33m&\x1b[30m/'
```

Dette vil skrive ut teksten fra nettsiden i kommandolinjen og formatere den med gult og svart. Du kan også bruke `curl` og `pup` for å gjøre samme jobben:

```Fish Shell
curl -s <nettside> | pup 'p.text{}'
```

Output vil være i ren tekst, uten formatering.

## Dykk dypere

En mer avansert måte å bruke Fish Shell for å parse HTML er å bruke verktøyet `xidel`. Dette verktøyet lar deg bruke XPath og CSS-selectors for å velge spesifikke elementer fra en nettside.

```Fish Shell
xidel <nettside> -e '//div[@class="innhold"]'
```

Dette eksemplet vil hente innholdet fra et HTML-element med klassen "innhold" på nettsiden. Du kan også utføre mer avanserte oppgaver som å kombinere flere XPath-forespørsler eller lagre utdataen i en variabel.

## Se også

- [Fish Shell dokumentasjon - Pipes og subshells](https://fishshell.com/docs/current/tutorial.html#tut_pipes)
- [Developer Mozilla - XPath tutorial](https://developer.mozilla.org/en-US/docs/Web/XPath)
- [CSS Diner - Et interaktivt CSS-selectors spill](https://flukeout.github.io/)