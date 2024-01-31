---
title:                "Scrivere test"
date:                  2024-01-19
html_title:           "Arduino: Scrivere test"
simple_title:         "Scrivere test"

category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Scrivere test significa creare piccole porzioni di codice che verificano il corretto funzionamento del proprio software. I programmatori li usano per assicurarsi che ogni parte del programma funzioni correttamente e per prevenire regressioni dopo modifiche o aggiornamenti.

## Come Fare:
Ecco un esempio semplice usando `clojure.test`, il framework di testing integrato in Clojure.

```Clojure
(require '[clojure.test :refer :all])

(deftest test-somma
  (testing "Verifica la funzione di somma"
    (is (= 10 (somma 3 7)))))

(run-tests)
```

Output:
```
Risultati dei test per namespace senza nome
Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

## Approfondimento
Clojure nasce intorno al 2007 e fin da subito integra `clojure.test` per promuovere Test-Driven Development (TDD). Esistono alternative come `Midje` e `Speclj` che offrono più funzionalità rispetto allo standard. Dettagli importanti da ricordare sono: scrivere test frequentemente, eseguirli dopo ogni cambiamento significativo e mantenere i test leggibili e manutenibili.

## Vedi Anche
- [Clojure Testing](https://clojure.org/guides/deps_and_cli#_testing) - una guida ufficiale.
- [Leiningen](https://leiningen.org/) - per gestire progetti e dipendenze in Clojure, incluso il testing.
- [Midje](https://github.com/marick/Midje) - un framework che facilita la scrittura di test.
- [Speclj](https://github.com/slagyr/speclj) - framework per BDD (Behavior-Driven Development) per Clojure.
