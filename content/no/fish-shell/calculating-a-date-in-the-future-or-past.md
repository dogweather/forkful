---
title:                "Fish Shell: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange ganger, når vi jobber med programmering eller planlegger aktiviteter, trenger vi å beregne en dato i fremtiden eller fortiden. Dette kan være for å planlegge et møte, en ferie eller for å sjekke når en vare skal leveres. Uansett grunn, er det en nyttig funksjon å kunne beregne datoer i Fish Shell.

## Hvordan gjøre det

For å beregne en dato i Fish Shell, kan vi bruke kommandoen `date`. La oss si at vi vil beregne en dato en uke fra nå. Vi kan bruke følgende kommando:

```
fish
date -v+1w
```

Dette vil gi oss datoen for én uke frem i tid.

Vi kan også spesifisere en startdato og legge til eller trekke fra dager, måneder eller år. For eksempel, la oss si at vi vil beregne datoen for tre måneder og to dager etter 15. september 2020. Vi kan bruke følgende kommando:

```
fish
date -v+3m -v+2d 20200915
```

Dette vil gi oss datoen 17. desember 2020.

## Dypdykk

Det er flere måter å formatere datoen på når du beregner den i Fish Shell. Du kan for eksempel bruke `-I` flagget for å få utgangen i ISO 8601-format, eller `-R` flagget for å få utgangen i RFC 2822-format.

I tillegg til å legge til eller trekke fra dager, måneder eller år, kan vi også spesifisere en spesifikk dato ved å bruke `-f` flagget. Dette kan være nyttig hvis vi for eksempel vil beregne datoen for en spesifikk dag i et annet år.

For mer informasjon om hvordan du bruker `date` kommandoen i Fish Shell, kan du se på dokumentasjonen på Fish Shell-nettstedet.

## Se også

- [Fish Shell - Dato dokumentasjon] (https://fishshell.com/docs/current/cmds/date.html)
- [ISO 8601] (https://www.iso.org/iso-8601-date-and-time-format.html)
- [RFC 2822] (https://tools.ietf.org/html/rfc2822)