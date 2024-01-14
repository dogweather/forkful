---
title:    "Clojure: Å bruke regulære uttrykk"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor bruke regulære uttrykk i Clojure? Regulære uttrykk er en svært kraftig funksjon i Clojure som gjør det mulig å søke eller manipulere tekst på en enkel og effektiv måte. Enten du jobber med dataanalyse, tekstbehandling eller webutvikling, vil du ha nytte av å lære hvordan du bruker regulære uttrykk i Clojure.

# Slik gjør du det

Å lære seg å bruke regulære uttrykk i Clojure kan virke skremmende ved første øyekast, men med litt øvelse vil du raskt få taket på det. La oss se på et eksempel der vi skal finne alle telefonnumre i en tekst:

```Clojure
(def tekst "Jeg kan nås på 12345678 eller 98765432")
(re-find #"[0-9]+" tekst)
```

I dette eksempelet definerer vi en tekststreng og bruker deretter funksjonen `re-find` sammen med regulært uttrykk #"[0-9]+" for å finne alle tallsekvenser i teksten. Resultatet vi får er "12345678", det første telefonnummeret som matcher mønsteret vårt.

Men hva om vi ønsker å finne både tall og bokstaver i teksten? Da må vi utvide vårt regulære uttrykk til å inkludere både tall og bokstaver:

```Clojure
(re-find #"[a-zA-Z0-9]+" tekst)
```

Dette vil gi oss resultatet "Jegkanås" og "98765432". Som du kan se, er det viktig å være presis med hva du ønsker å finne med regulære uttrykk.

# Dypdykk

Hvis du ønsker å lære mer om hvordan du bruker regulære uttrykk i Clojure, kan du sjekke ut dokumentasjonen for Clojure-regex-biblioteket og få mer informasjon om hvilke funksjoner du kan bruke for å søke og manipulere tekst. Du kan også øke dine ferdigheter ved å utforske ulike typer regulære uttrykk og se hvordan de fungerer.

# Se også

- [Clojure-regex dokumentasjon](https://clojure.github.io/clojure.contrib/regex-api.html)
- [Clojure regex-hjelp](https://juxt.pro/blog/clojure-regex-guide.html)