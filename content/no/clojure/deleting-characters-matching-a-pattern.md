---
title:                "Clojure: Sletting av tegn som matcher et mønster"
simple_title:         "Sletting av tegn som matcher et mønster"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger kan det hende vi trenger å slette spesifikke tegn som matcher et mønster i en tekststreng. Dette kan være nyttig når vi trenger å rense data eller filtrere ut uønskede tegn. I denne bloggposten vil vi utforske hvordan vi kan gjøre dette ved hjelp av Clojure-programmering.

## Hvordan

For å slette tegn som matcher et mønster, kan vi bruke funksjonen `clojure.string/replace` i Clojure. Denne funksjonen tar inn en tekststreng, et mønster og en erstatning og returnerer en ny tekststreng med de matchende tegnene erstattet med ønsket erstatning.

```Clojure
(clojure.string/replace "Dette er en tekst som inneholder tall 123" #"[\d]" "")
; "Dette er en tekst som inneholder tall "
```

Her sletter vi alle tall i tekststrengen ved å bruke mønsteret `#"[d]"` som matcher alle tall i strengen. Vi erstatter de matchende tegnene med et tomt tegn `""` som bare sletter dem.

La oss se et annet eksempel der vi ønsker å slette alle vokaler fra en tekststreng:

```Clojure
(clojure.string/replace "Dette er en tekst med vokaler" #"aeiou" "")
; "Dtt r n txt m vklr"
```

I dette tilfellet bruker vi mønsteret `#"aeiou"` som matcher alle vokaler i tekststrengen. Vi erstatter de matchende tegnene med et tomt tegn `""` for å slette dem.

## Gå i dybden

Det er også mulig å bruke regulære uttrykk i mønsteret for å utføre mer avansert sletting av tegn. Vi kan for eksempel bruke `#"[\p{Punct}]"` for å slette alle tegn som regnes som tegnsetting i teksten. Dette vil inkludere tegn som komma, punktum, utropstegn osv.

Vi kan også bruke funksjonen `clojure.string/replace-first` for å bare slette det første mønsteret som matcher i tekststrengen. Dette kan være nyttig når vi bare ønsker å fjerne en spesifikk forekomst av et tegn eller et mønster.

```Clojure
(clojure.string/replace-first "Dette er en tekst med flere tegn !" #"tegn" "")
; "Dette er en tekst med flere !"
```

Som du kan se, har bare den første forekomsten av "tegn" blitt slettet, selv om det var flere.

## Se også

- Clojure dokumentasjon for `clojure.string/replace` og `clojure.string/replace-first`: https://clojuredocs.org/clojure.string/replace, https://clojuredocs.org/clojure.string/replace-first
- Regulære uttrykk i Clojure: https://clojuredocs.org/clojure.repl/doc
- Bruk av Clojure funksjoner i strenger: https://clojure.or.id/en/slot/2011/04/clojure-core-important-library-string-function/