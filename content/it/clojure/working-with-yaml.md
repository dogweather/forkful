---
title:                "Clojure: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

Italian Translation:

## Perché

Ci sono molte ragioni per cui un programmatore potrebbe voler lavorare con YAML in Clojure. YAML è un formato di file molto flessibile e leggibile per i dati strutturati, il che lo rende ideale per la configurazione di applicazioni e l'interscambio di dati tra sistemi. Inoltre, YAML è ampiamente supportato dalla comunità di programmazione e ha molte librerie disponibili in Clojure.

## Come fare

Utilizzando la libreria `yaml-clj`, è abbastanza semplice leggere e scrivere file YAML in Clojure. Ecco un esempio di come leggere un file YAML e stampare il contenuto:

```Clojure
(require '[yaml-clj.core :as yaml])

(def config (yaml/load-file "config.yaml"))

(println (:db-uri config))
```

Questo codice carica il file `config.yaml` e lo memorizza nella variabile `config`. Successivamente, viene utilizzata la sintassi delle parentesi graffe per accedere al valore `db-url` all'interno del file YAML. Infine, il valore viene stampato a schermo.

Ecco un esempio di come scrivere un file YAML da Clojure:

```Clojure
(yaml/write-file "config.yaml" {:username "john" :password "secret"})
```

In questo caso, il file `config.yaml` conterrà i dati specificati in formato YAML.

## Approfondimento

Una delle principali caratteristiche di YAML è la sua capacità di rappresentare dati in forma più strutturata rispetto ad altri formati come JSON e CSV. Questo significa che è più facile per gli umani leggere e scrivere, rendendolo adatto per la configurazione e la gestione dei dati complessi.

Una delle sfide di lavorare con YAML in Clojure è gestire i valori null e le liste datile, che non sono supportati come tali in Clojure. Tuttavia, ci sono diverse librerie disponibili che aiutano a gestire questi casi, come `yaml-clj-null` per i valori null e `yaml-clj-sequence` per le liste datile.

## Vedi anche

- [Documentazione ufficiale di YAML](https://yaml.org/)
- [Libreria di Clojure per la gestione di file YAML](https://github.com/yaml-clj/yaml-clj)
- [Libreria per la gestione di valori null in file YAML](https://github.com/yaml-clj/yaml-clj-null)
- [Libreria per la gestione di liste datile in file YAML](https://github.com/yaml-clj/yaml-clj-sequence)