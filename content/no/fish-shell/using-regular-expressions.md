---
title:    "Fish Shell: Å bruke regulære uttrykk"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skal du bruke regulære uttrykk når du programmerer? Svaret er enkelt: det er en effektiv måte å søke og manipulere tekst på. Enten du utvikler et nettsted, prosesserer data eller bare redigerer tekstfiler, er regulære uttrykk et kraftig verktøy å ha i verktøykassen din.

## Slik gjør du det

For å bruke regulære uttrykk i Fish Shell, kan du enkelt bruke kommandoen `grep`. For eksempel, la oss si at du har en tekstfil som heter "navnliste.txt" som inneholder følgende navn:

```
Kari
Ole
Sofie
Erik
```

Hvis du vil søke etter alle navn som inneholder bokstaven "e", kan du bruke følgende kommando:

```
grep "e" navnliste.txt
```

Dette vil gi følgende output:

```
Kari
Sofie
Erik
```

Du kan også bruke regulære uttrykk for å søke etter mønstre. For eksempel, hvis du bare ønsker å få ut navn som starter med bokstaven "S", kan du bruke følgende kommando:

```
grep "^S" navnliste.txt
```

Dette vil bare gi deg følgende resultat:

```
Sofie
```

## Dykk dypere

For å få enda mer ut av regulære uttrykk, kan du utforske flere metakarakterer og spesialtegn. En av de mest nyttige er `*`, som betyr "null eller flere forekomster av det forrige tegnet". For eksempel, hvis du vil søke etter navn som inneholder "Erik" eller "Eirik", kan du bruke følgende kommando:

```
grep "Ei*rik" navnliste.txt
```

Dette vil gi deg følgende output:

```
Erik
Eirik
```

Det er også andre metakarakterer som `+` (én eller flere forekomster av det forrige tegnet) og `?` (null eller én forekomst av det forrige tegnet). Å eksperimentere med disse kan hjelpe deg å lage mer avanserte søkemønstre.

## Se også

Få mer informasjon om regulære uttrykk og hvordan du bruker dem i Fish Shell her:

- [Fish Shell Tutorial: Regular Expressions](https://fishshell.com/docs/current/tutorial.html#tut_regex)
- [Learning Regular Expressions with Fish Shell](https://dev.to/ishworgurung/learning-regular-expressions-with-fish-shell-188p)

Lykke til med å utforske de utallige mulighetene med regulære uttrykk i Fish Shell!