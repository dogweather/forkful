---
aliases:
- /no/clojure/writing-tests/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:02.482478-07:00
description: "\xC5 skrive tester i Clojure, mye som i andre programmeringsspr\xE5\
  k, inneb\xE6rer \xE5 lage dedikert kode for \xE5 verifisere at hovedkodebasen din\
  \ fungerer som\u2026"
lastmod: 2024-02-18 23:08:53.567837
model: gpt-4-0125-preview
summary: "\xC5 skrive tester i Clojure, mye som i andre programmeringsspr\xE5k, inneb\xE6\
  rer \xE5 lage dedikert kode for \xE5 verifisere at hovedkodebasen din fungerer som\u2026"
title: Skrive tester
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive tester i Clojure, mye som i andre programmeringsspråk, innebærer å lage dedikert kode for å verifisere at hovedkodebasen din fungerer som forventet. Det bidrar til å sikre riktighet, lette refaktorering, og forbedre kode stabilitet.

## Hvordan:
Clojure, ved å dra nytte av JVM, støtter forskjellige testrammeverk. Imidlertid er et ofte brukt innebygd bibliotek `clojure.test`. Her er et enkelt eksempel:

```clojure
(ns example.test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))

(deftest test-addition
  (testing "Addisjonsfunksjonalitet"
    (is (= 4 (add 2 2)))
    (is (= 7 (add 3 4)))))

(run-tests)
```
Etter å ha kjørt denne testen, ville du se en utskrift som ligner på:

```
Tester example.test

Kjørte 2 tester som inneholdt 2 påstander.
0 feil, 0 feil.
```

For de som søker mer funksjonsrike alternativer, kan man utnytte tredjepartsbiblioteker som `Midje` eller `test.check`. Slik kan du bruke Midje for en lignende test:

Først, legg til Midje i dine project.clj avhengigheter:
```clojure
[midje "1.9.9"]
```

Deretter kan testen din med Midje se slik ut:

```clojure
(ns example.test
  (:require [midje.sweet :refer :all]
            [example.core :refer :all]))

(fact "Tester addisjon"
  (add 2 2) => 4
  (add 3 4) => 7)
```

Ved å kjøre testen gjennom Midje med `lein midje`, ville utskriften vise noe liknende:

```
Alle sjekker (2) lyktes.
```
