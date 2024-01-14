---
title:                "Fish Shell: Sletting av tegn som matcher et mønster"
simple_title:         "Sletting av tegn som matcher et mønster"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger kan det være nyttig å slette tegn som matcher et visst mønster i en tekststreng. Dette kan gjøres raskt og enkelt med Fish Shell.

## Hvordan

For å slette tegn som matcher et mønster i Fish Shell, kan du bruke kommandoen `string delete -r`. La oss si at du har en tekststreng `hello123world`, og du vil slette alle tallene. Da kan du bruke følgende kommando:

```
fish shell: `string delete -r '\d' hello123world`

Output: helloworld
```

Dette fjerner alle tallene fra teksten og returnerer `helloworld`.

## Dykk dypere

I tillegg til å bruke regex, kan du også bruke `string delete`-kommandoen til å slette tegn som matcher et bestemt sett av tegn eller enkelttegn i en tekststreng. For eksempel hvis du vil slette alle de små bokstavene i teksten, kan du bruke følgende kommando:

```
fish shell: `string delete a-z hello123world`

Output: 123.
```

Dette vil returnere `123`, og slette alle små bokstaver fra teksten.

Fish Shell har også muligheten til å slette tegn fra bestemte posisjoner i en tekststreng ved hjelp av indeksering.

## Se også

- [Fish Shell hjemmeside](https://fishshell.com/)
- [Offisiell Fish Shell dokumentasjon](https://fishshell.com/docs/current/)
- [Learning Fish](https://fishshell.com/docs/current/tutorial.html)