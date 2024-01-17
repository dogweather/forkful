---
title:                "Søking og erstatning av tekst"
html_title:           "Clojure: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?

Søk og erstatt tekst er en vanlig oppgave for dataprogrammerere. Det handler om å finne og erstatte et spesifikt uttrykk i en tekstfil eller datamaskinkode. Dette gjøres for å forenkle og effektivisere programmeringsprosessen ved å gjøre store endringer i koden på en enkel måte.

## Hvordan:

For å søke og erstatte tekst i Clojure, kan du bruke funksjonen "replace". Denne funksjonen tar inn to argumenter: en tekststreng som den skal søke etter, og en tekststreng som skal erstatte den funne teksten. Se et eksempel nedenfor:

```Clojure
(replace "hund" "katt" "Jeg har en stor hund")
```

Dette vil resultere i teksten "Jeg har en stor katt".

Du kan også bruke regulære uttrykk for å søke etter mer komplekse mønstre i teksten. For eksempel:

```Clojure
(replace #"([0-9]+)st" "$1nd" "1st, 2nd, 3rd")
```

Dette vil resultere i teksten "1st, 2nd, 3rd" siden den finner alle forekomster av ordet "st" etter et tall og erstatter det med "nd".

## Dypdykk:

Søk og erstatte funksjonalitet har vært tilgjengelig i programmeringsspråk siden begynnelsen. Alternativer til Clojure inkluderer Java Regex, Python Regex og Perl Regex.

I Clojure er søk og erstatte implementert ved hjelp av Java-biblioteket java.util.replace.

## Se også:

- Clojure's regex bibliotek: https://clojuredocs.org/clojure.core/re-matcher