---
title:                "Skrive tester"
html_title:           "Clojure: Skrive tester"
simple_title:         "Skrive tester"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/writing-tests.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?
Å skrive tester er en viktig del av programmering, hvor det er en måte å sikre at koden vi skriver fungerer som den skal. Testene våre kan også hjelpe oss med å identifisere eventuelle feil eller mangler i koden før den blir implementert i et reelt system.

Hvordan:
La oss se på et enkelt eksempel på å skrive en test i Clojure:

```Clojure
(ns test-eksempel
  (:require [clojure.test :refer [is]]))

(defn legg-sammen [a b]
  (+ a b))

(deftest test-legg-sammen
  (is (= 4 (legg-sammen 2 2))))

(defn multipliser [a b]
  (* a b))

(deftest test-multipliser
  (is (= 12 (multipliser 3 4))))

```

Vi starter med å importere "clojure.test" biblioteket som gir oss funksjonaliteten til å definere tester. Deretter definerer vi to funksjoner, "legg-sammen" og "multipliser", og skriver en test for hver av dem ved hjelp av "deftest" funksjonen og "is" asserter.

Når vi kjører disse testene, vil vi få følgende utgang:

```Clojure
lein test

Ran 2 tests containing 2 assertions.
0 failures, 0 errors.
```

Deep Dive:
Tester har vært en viktig praksis i programmering i lang tid, og det finnes flere alternativer til å skrive tester i Clojure, som f.eks. den populære "Midje" testing-rammen. Clojure's standard testing-bibliotek, "clojure.test", er fortsatt svært brukt på grunn av sin enkelhet og effektivitet.

Når vi skriver tester i Clojure, følger vi vanligvis en test-drevet utviklingsprosess. Dette betyr at vi først skriver en test som feiler, så implementerer vi kode som gjør testen passere, og til slutt refaktorerer vi koden om nødvendig. Dette sikrer at koden vår fungerer som forventet og reduserer sannsynligheten for feil.

See Also:
- [Clojure.test dokumentasjon] (https://clojure.org/guides/testing).
- [Midje dokumentasjon] (https://github.com/marick/Midje).
- [Test-driven utvikling] (https://en.wikipedia.org/wiki/Test-driven_development).