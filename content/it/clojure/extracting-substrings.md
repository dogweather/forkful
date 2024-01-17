---
title:                "Estrazione di sottostringhe"
html_title:           "Clojure: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
L'estrazione di sottostringhe è il processo di ottenere una stringa più piccola da una stringa più grande. I programmatori spesso lo fanno per manipolare i dati in modo più efficiente e per ottenere solo le parti della stringa che sono pertinenti al loro scopo.

## Come fare:
Utilizzando la funzione ```subs``` di Clojure, possiamo facilmente estrarre una sottostringa da una stringa. Passando alla funzione un parametro che indica l'inizio e un parametro che indica la fine della sottostringa desiderata, otterremo il risultato desiderato. Ad esempio:

```Clojure
(subs "Hello world" 0 5)
=> "Hello"
```

Possiamo anche utilizzare le funzioni di stringa di base, come ```substring```, ```take``` e ```drop```, per ottenere sottostringhe in modi diversi. Ad esempio:

```Clojure
(substring "Hello world" 6)
=> "world"

(take 3 "Hello world")
=> "Hel"

(drop 6 "Hello world")
=> "world"
```

## Approfondimento:
L'estrazione di sottostringhe è un'operazione comune nella programmazione ed è stata utilizzata fin dai primi giorni dei linguaggi di programmazione. Ci sono anche altre alternative, come l'utilizzo di espressioni regolari o il parsing di dati strutturati. Inoltre, le sottostringhe possono anche essere estratte utilizzando gli indici dei caratteri anziché la lunghezza dei caratteri.

## Vedi anche:
Per ulteriori informazioni su come estrarre sottostringhe in Clojure, si consiglia di consultare la documentazione ufficiale su come lavorare con le stringhe: https://clojure.org/guides/strings.