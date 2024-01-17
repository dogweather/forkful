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

# Cosa & Perché?
Lavorare con YAML significa gestire dati strutturati in formato testo, utilizzato principalmente per configurazioni e scambi di dati. I programmatori lo utilizzano per rendere le informazioni leggibili e facilmente manipolabili.

# Come:
I codici di esempio di seguito mostrano l'utilizzo di YAML in Clojure.

```Clojure
(require '[clojure.data.yaml :as yaml])
(require '[clojure.java.io :as io])
(yaml/write-str {:nome "Mario" :età 30})
```

```Clojure
(require '[clojure.data.yaml :as yaml])
(require '[clojure.java.io :as io])
(binding [*default-data-writer-fn* nil] (yaml/read-str (slurp "esempio.yml")))
```

Ecco l'output del primo codice:

```Clojure
"eta: 30\ nome: Mario\n"
```

# Approfondimento:
YAML è stato sviluppato da Clark Evans nel 2001, con l'obiettivo di creare un formato di dati più leggibile per le persone e più facile da scrivere per i programmi. Alcune alternative a YAML includono JSON e XML. 
L'implementazione di YAML in Clojure è basata sul pacchetto "snakeyaml", che offre funzionalità complete per la lettura e scrittura di file YAML.

# Vedi anche:
Per ulteriori informazioni su YAML in Clojure, puoi consultare la documentazione ufficiale su https://github.com/clj-commons/data.yaml. Inoltre, puoi esplorare altre librerie di terze parti, come "clj-yaml" e "data-reader-yaml", per sfruttare al massimo l'utilizzo di YAML nella tua programmazione.