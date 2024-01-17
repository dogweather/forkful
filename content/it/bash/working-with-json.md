---
title:                "Lavorare con json"
html_title:           "Bash: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Lavorare con JSON significa gestire dati strutturati in formato testuale. Questo formato è ampiamente utilizzato dai programmatori perché è compatto e facile da leggere e scrivere.

## Come fare:

Un modo semplice per lavorare con JSON in Bash è utilizzare il comando `jq` che è incluso nella maggior parte delle distribuzioni Linux. Ecco un esempio per stampare un valore specifico dal file JSON:

```Bash
jq '.key' file.json
```

L'output sarà il valore corrispondente alla chiave specificata.

## Approfondimento:

JSON è diventato rapidamente uno dei formati più popolari per lo scambio di dati grazie alla sua semplicità e flessibilità. Tuttavia, ci sono alternative come XML e YAML che possono essere utilizzate a seconda delle esigenze. Per lavorare con JSON in Bash, è possibile utilizzare anche strumenti come `sed` e `awk` che offrono maggiori possibilità di manipolazione dei dati.

## Vedi anche:

Per maggiori informazioni su JSON e come lavorare con esso in Bash, puoi consultare la documentazione ufficiale di [jq](https://stedolan.github.io/jq/) e [GNU sed](https://www.gnu.org/software/sed/). Inoltre, puoi trovare utili esempi e tutorial su siti come [DigitalOcean](https://www.digitalocean.com/community/tutorials/an-introduction-to-jq) e [Linuxize](https://linuxize.com/post/processing-json-in-bash-with-jq/).