---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Clojure: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bry seg med å konvertere en streng til små bokstaver? Vel, dette er et vanlig scenarie i programmering hvor du kanskje trenger å sammenligne eller søke gjennom tekst. Ved å konvertere en streng til små bokstaver, kan du sikre at du får korrekte resultater uten å bekymre deg for store og små bokstaver.

## Slik gjør du det

For å konvertere en streng til små bokstaver i Clojure, bruker du funksjonen `clojure.string/lower-case`:

```Clojure
(def s "Tekst Til Konvertering")
(clojure.string/lower-case s)
```

Output:

```Clojure
"tekst til konvertering"
```

En annen måte å gjøre dette på er ved å bruke `case`-funksjonen, som lar deg spesifisere hvilken type konvertering du vil gjøre. For å konvertere en streng til små bokstaver bruker du `:lower` som argument:

```Clojure
(def s "Tekst Til Konvertering")
(clojure.string/case s :lower)
```

Output:

```Clojure
"tekst til konvertering"
```

## Dypdykk

Hvis du vil forstå hvordan disse funksjonene fungerer under panseret, kan du tenke på det som å konvertere hver enkelt bokstav i strengen til sitt tilsvarende tilfelle. For eksempel, hvis strengen inneholder store bokstaver, vil funksjonen bytte ut hver bokstav med sin tilsvarende små bokstav.

Det er også viktig å huske på at disse funksjonene følger Unicode-standardene, som betyr at bokstaver fra andre språk også vil bli konvertert til små bokstaver (hvis de har en små bokstav-variant).

## Se også

- Clojure Docs: `clojure.string/lower-case` [https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/lower-case](https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/lower-case)
- Clojure Docs: `clojure.string/case` [https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/case](https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/case)