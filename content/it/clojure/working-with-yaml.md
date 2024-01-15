---
title:                "Lavorare con yaml"
html_title:           "Clojure: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore che lavora con dati strutturati, probabilmente hai sentito parlare di YAML. Questo formato di file è ampiamente utilizzato per gestire configurazioni e dati complessi in modo leggibile per le persone e facilmente parsabile dai computer. In questo articolo, esploreremo come utilizzare Clojure per lavorare con YAML e ottenere il massimo da questo potente strumento.

## Come fare

Il primo passo per lavorare con YAML in Clojure è importare la libreria `clojure-yaml` nel nostro progetto. Possiamo farlo con il seguente codice:

```Clojure
(ns my-project.core
  (:require [clojure-yaml.core :as yaml]))
```

Una volta importata la libreria, possiamo utilizzare la sua funzione `parse-string` per convertire una stringa YAML in una mappa Clojure. Ad esempio:

```Clojure
(def yaml-string "# Questo è un commento valido in YAML\nnome: John Smith\netà: 30")
(yaml/parse-string yaml-string)
;; output: {:nome "John Smith", :età 30}
```

In alternativa, possiamo utilizzare la funzione `emit-string` per convertire una mappa Clojure in una stringa YAML. Ad esempio:

```Clojure
(def data {:numero 10, :lista [1 2 3], :map {"chiave" "valore"}})
(yaml/emit-string data)
;; output: "{numero: 10, lista: [1, 2, 3], map: {chiave: valore}}"
```

Una volta che abbiamo i nostri dati in formato YAML, possiamo utilizzare la funzione `write-file` per salvarli su un file. Ad esempio:

```Clojure
(yaml/write-file "config.yaml" {:nome "John Smith", :attivo true})
```

## Approfondimento

YAML supporta diversi tipi di dati, tra cui interi, decimali, stringhe, booleani, date e altro ancora. Per ulteriori informazioni sui tipi di dati supportati e sulla sintassi di YAML, puoi consultare la sua documentazione ufficiale.

Inoltre, la libreria Clojure per YAML supporta anche opzioni avanzate come la gestione di riferimenti tra diversi documenti YAML e la personalizzazione del processo di conversione. Puoi esplorare questi aspetti maggiormente nella documentazione della libreria.

## Vedi anche

- Documentazione ufficiale di YAML: https://yaml.org/
- Documentazione della libreria Clojure per YAML: https://github.com/novemberborn/clojure-yaml