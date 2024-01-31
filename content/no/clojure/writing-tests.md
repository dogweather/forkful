---
title:                "Skriving av tester"
date:                  2024-01-19
html_title:           "Arduino: Skriving av tester"
simple_title:         "Skriving av tester"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skrive tester er å kode små sjekker som validerer at programkode fungerer som den skal. Programmerere gjør dette for å fange feil tidlig, forenkle vedlikehold og sikre kvaliteten over tid.

## Gjør sånn:
Test biblioteket `clojure.test` er standard for testing. Her er et eksempel:

```Clojure
(require '[clojure.test :refer :all])

(deftest add-test
  (testing "Addisjon av to tall"
    (is (= 4 (+ 2 2)))))
  
(run-tests)
```

Output blir:

```
Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

## Dybde:
Clojure tester har røtter i Lisp tradisjonen, bygd for funksjonelle paradigmer. Alternativer inkluderer `Midje` og `Expectations`. Clojure's `deftest` makro lar deg gruppere og merke testtilfeller. Du kan også bruke `fixtures` for å sette opp og rive ned testmiljøet.

## Se Også:
- [Clojure Testing](https://clojure.org/guides/deps_and_cli#_testing)
- [ClojureDocs - clojure.test](https://clojuredocs.org/clojure.test)
- [Practicalli - Testing Clojure Code](https://practicalli.github.io/clojure/testing/)
