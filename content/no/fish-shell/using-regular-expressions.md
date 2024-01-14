---
title:    "Fish Shell: Å bruke regulære uttrykk"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skal man bruke regulære uttrykk i programmering? Regulære uttrykk er et kraftig verktøy for å finne, filtrere og manipulere tekst og gjør det enklere og raskere å utføre komplekse søk og handlinger.

## Hvordan

Å bruke regulære uttrykk i Fish Shell er enkelt og effektivt. Her er noen eksempler på hvordan du kan bruke det:

```Fish Shell
# Søk etter alle linjer som inneholder ordet "fisk"
grep "fisk" filnavn.txt

# Finn alle tall i en tekststreng
grep -E "[0-9]+" filnavn.txt

# Endre alle forekomster av "sea" til "ocean" i en tekstfil
sed -i "s/sea/ocean/g" filnavn.txt
```

Output for det siste eksempelet vil bli:

```Fish Shell
Dype havet er fylt med liv og mysterier.
```
til
```Fish Shell
Dype oceanet er fylt med liv og mysterier.
```

## Dykke Dypere

Regulære uttrykk har et stort utvalg av spesialtegn som kan brukes for å gjøre søk mer spesifikke og nøyaktige. Du kan også kombinere forskjellige uttrykk for å lage mer komplekse søk.

En annen nyttig funksjon med regulære uttrykk er at du kan bruke dem sammen med andre kommandoer i Fish Shell, som for eksempel sed, awk og cut. Dette gir deg muligheten til å manipulere tekst på flere forskjellige måter og å automatisere oppgaver som tidligere kunne vært tidkrevende og kjedelige.

Før du begynner å bruke regulære uttrykk, er det viktig å forstå hvordan de fungerer og å øve deg på å lage dem. Det finnes mange online ressurser og bøker som kan hjelpe deg med å lære mer om dette kraftige verktøyet.

## Se Også

- [Fish Shell's offisielle nettside](https://fishshell.com/)
- [Regulære uttrykk tutorial fra W3Schools](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [Bruke regulære uttrykk i Fish Shell blogginnlegg fra Stack Abuse](https://stackabuse.com/using-regular-expressions-in-fish-shell/)