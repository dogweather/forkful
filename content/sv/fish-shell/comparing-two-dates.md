---
title:    "Fish Shell: Jämföring av två datum"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Varför

När vi arbetar med datum i programmering kan det ibland vara nödvändigt att jämföra två datum för att avgöra exempelvis vilket datum som är tidigare eller senare. Fish Shell erbjuder en enkel och effektiv metod för att göra detta, vilket kan vara användbart i många olika programmeringsprojekt.

## Så här gör du

För att jämföra två datum i Fish Shell, används kommandot ```date -d <datum 1> -ge <datum 2>```, där <datum 1> och <datum 2> ersätts med de två datum som önskas jämföras.

Exempel:

```
date -d '2020-01-01' -le '2021-01-01'
```

Första datumet (2020-01-01) är äldre än det andra datumet (2021-01-01), så resultatet kommer att bli "False". Om vi byter plats på datumen får vi istället ett resultat av "True", eftersom det första datumet då är senare än det andra.

Det finns även möjlighet att jämföra tidpunkter inom samma datum genom att lägga till tiden efter datumet inom citattecknen.

Exempel:

```
date -d '2021-05-01 12:00:00' -gt '2021-05-01 10:00:00'
```

I detta exempel är tidpunkten 12:00:00 senare än 10:00:00, så resultatet kommer att bli "True".

## Djupdykning

Vid jämförelse av datum i Fish Shell används datuminformationen som visas när man skriver kommandot ```date```, vilket kan skilja sig åt beroende på vilket system man kör på. Om man vill vara säker på att få önskade resultat rekommenderas att man formaterar datumen enligt ISO 8601-standard, det vill säga yyyy-mm-dd.

Ytterligare användbara flaggor för kommandot ```date``` är "-s" för att sätta datum och tid (om man till exempel vill jämföra med det nuvarande datumet), och "-r" för att läsa in datum från en fil.

## Se också

- [Datumspecifikation i Fish Shell dokumentation](https://fishshell.com/docs/current/commands.html#command-substitution)
- [ISO 8601-standard för datumformatering](https://www.iso.org/iso-8601-date-and-time-format.html)
- [Mer information om "date" kommandot i Fish Shell](https://fishshell.com/docs/current/commands.html#date)