---
title:                "Bash: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Bash programmering for å beregne datoer

## Hvorfor

Beregning av datoer i fremtiden eller fortiden er en vanlig oppgave for mange. Det kan være nyttig for å planlegge hendelser, som bursdager eller ferier, eller for å spore markeder og aksjer. Uansett årsak er det viktig å ha en enkel og pålitelig måte å beregne datoer på, og dette er akkurat det Bash-programmering kan hjelpe oss med.

## Hvordan 

Det finnes flere forskjellige måter å beregne datoer i fremtiden eller fortiden ved hjelp av Bash-programmering. Et eksempel er å bruke kommandoen "date" kombinert med et antall dager, måneder eller år som skal legges til eller trekkes fra den nåværende datoen. For å beregne datoen 10 dager frem i tid, kan du for eksempel bruke kommandoen "date -d '+10 days'", og du vil få output som viser datoen 10 dager etter dagens dato.

```Bash
$ date -d '+10 days'
Onsdesdag 12. mai 2021 21.54.00 CEST
```

Du kan også bruke variabler og matematiske operasjoner for å beregne datoer. For eksempel kan du bruke variabelen "today" som er satt til dagens dato, og deretter bruke dette til å beregne datoen 3 uker frem i tid. Resultatet vil være det samme som ved å bruke "date" kommandoen, men dette er et eksempel på hvordan du kan bruke Bash-programmering for å gjøre beregningene mer fleksible.

```Bash
$ today=$(date +%Y-%m-%d)
$ echo $today
2021-05-02
$ new_date=$(date -d "-1 month +3 weeks" '+%Y-%m-%d')
$ echo $new_date
2021-05-23
```

## Deep Dive

Hvis du ønsker å lære mer om hvordan Bash-programmering beregner datoer, kan du utforske "date" kommandoen nærmere. Her er noen av de mest nyttige argumentene du kan bruke for å beregne datoer i fortid og fremtid:

- -d/--date: Dette argumentet lar deg angi en spesifikk dato i stedet for dagens dato. For eksempel kan du bruke "date -d '2021-07-15' '+3 months'" for å beregne datoen 3 måneder etter 15. juli 2021.
- -v/--version: Dette argumentet lar deg utføre mer komplekse beregninger ved å bruke variabler og matematiske operasjoner. For eksempel kan du bruke "date -v '+1d' '+%Y-%m-%d'" for å beregne datoen for neste dag.
- -u/--utc: Dette argumentet viser datoen og klokkeslettet i UTC-tidssone i stedet for den lokale tidssonen.
- -j/--day: Dette argumentet returnerer dagen i året i stedet for dato og klokkeslett.
- -r/--reference: Dette argumentet lar deg bruke en referansedato og beregne forskjellen mellom den datoen og dagens dato.

Se også

- [Bash manual page for date command](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Bash Scripting Tutorial](https://ryanstutorials.net/bash-scripting-tutorial/bash-variables.php)
- [Unix Timestamp Converter](https://www.unixtimestamp.com/)

---

Bash-programmering kan være et nyttig verktøy for å beregne datoer i fremtiden eller fortiden. Ved å bruke enkle kommandoer som "date" kan du enkelt få output med ønsket dato og klokkeslett. Du kan også utforske mer avanserte alternativer for å gjøre beregningene mer nøyaktige og fleksible. Lykke til med å beregne dine fremtidige arrangementer og følg med på markeder og aksjer med Bash-programmering!