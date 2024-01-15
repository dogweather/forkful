---
title:                "Lavorare con yaml"
html_title:           "Fish Shell: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché

Se stai cercando un modo semplice e veloce per gestire dati strutturati nel tuo terminale, allora la programmazione con YAML in Fish Shell potrebbe essere la soluzione perfetta per te. Con questa guida, imparerai come utilizzare YAML in modo efficiente e intuitivo, senza dover ricorrere a strumenti esterni.

## Come fare

Per iniziare a utilizzare YAML in Fish Shell, è necessario prima installare il plugin "fish-yaml". Puoi farlo comodamente utilizzando il gestore dei pacchetti "fisher". Una volta installato, basta importare il modulo in Fish Shell con il comando:

```
fisher install jorgebucaran/fish-yaml
```

Una volta importato il modulo, puoi iniziare a gestire dati YAML nel tuo terminale. Ad esempio, creando un file YAML come questo:

```
nome: Mario
cognome: Rossi
età: 38
```

Puoi poi utilizzare il comando `yaml_print` per visualizzare i dati nel file come output:

```
$ yaml_print nome
Mario
$ yaml_print cognome
Rossi
$ yaml_print età
38
```

In questo modo puoi facilmente accedere ai dati strutturati senza doverli estrarre manualmente dal file.

## Deep Dive

Oltre alla semplice gestione dei dati, YAML in Fish Shell offre anche funzioni avanzate per il parsing e la manipolazione dei dati. Ad esempio, puoi utilizzare il comando `yaml_parse` per convertire un file YAML in un array associativo, che ti consente di accedere ai dati in modo ancora più efficiente.

Puoi anche utilizzare funzioni come `yaml_set` e `yaml_delete` per modificare i dati all'interno di un file YAML direttamente dal tuo terminale. Inoltre, il linguaggio espressivo di YAML ti permette di creare strutture dati complesse, rendendo questo strumento utile per una vasta gamma di progetti.

## Vedi anche
- [Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/current/index.html)
- [fish-yaml repository su GitHub](https://github.com/jorgebucaran/fish-yaml)
- [Tutorial sulle basi di YAML](https://www.tutorialsteacher.com/yaml)