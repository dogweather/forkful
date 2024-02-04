---
title:                "Scrivere test"
date:                  2024-02-03T19:30:02.320492-07:00
model:                 gpt-4-0125-preview
simple_title:         "Scrivere test"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?
Scrivere test in Clojure, così come in altri linguaggi di programmazione, comporta la creazione di codice dedicato per verificare che il tuo codice principale funzioni come previsto. Aiuta ad assicurare la correttezza, facilita il refactoring e migliora la stabilità del codice.

## Come fare:
Clojure, sfruttando la JVM, supporta vari framework di testing. Tuttavia, una libreria integrata comunemente usata è `clojure.test`. Ecco un esempio semplice:

```clojure
(ns example.test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))

(deftest test-addizione
  (testing "Funzionalità di addizione"
    (is (= 4 (add 2 2)))
    (is (= 7 (add 3 4)))))

(run-tests)
```
Dopo aver eseguito questo test, vedresti un output simile a:

```
Testing example.test

Eseguiti 2 test contenenti 2 affermazioni.
0 fallimenti, 0 errori.
```

Per coloro che cercano opzioni più ricche di funzionalità, si può utilizzare librerie di terze parti come `Midje` o `test.check`. Ecco come potresti usare Midje per un test simile:

Prima, aggiungi Midje alle dipendenze del tuo project.clj:
```clojure
[midje "1.9.9"]
```

Poi, il tuo test con Midje potrebbe apparire così:

```clojure
(ns example.test
  (:require [midje.sweet :refer :all]
            [example.core :refer :all]))

(fact "Test dell'addizione"
  (add 2 2) => 4
  (add 3 4) => 7)
```

Eseguendo il test tramite Midje con `lein midje`, l'output visualizzerebbe qualcosa del tipo:

```
Tutti i controlli (2) sono riusciti.
```
