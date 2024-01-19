---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/extracting-substrings.md"
---

{{< edit_this_page >}}

# Un Tuffo nel Substring Extraction con Python

## Cosa e Perché?

L'estrazione delle sottostringhe si riferisce al processo per il quale estraiamo una parte più piccola di una stringa stando al suo indice. È spesso utilizzato dai programmatori per manipolare e analizzare i dati in modo più efficiente.

## Come Fare:

```Python
# Dichiarazione di una stringa in Python
s = "Benvenuti a Italian Programming"

# Estrazione di una sottostringa
substring = s[10:21]

# Stampiamo la sottostringa
print(substring)
```

Ecco l'output:

```Python
Italian Pro
```

## Approfondimento

Nel contesto storico, le operazioni di sottostringa risalgono agli anni '60, quando le operazioni sulle stringhe sono diventate fondamentali nell'ambito della programmazione. 

Esistono alternative all'estrazione di sottostringhe in Python? Certo! Puoi utilizzare metodi built-in come `split()` o library esterne come `regex` per eseguire compiti simili. 

Relativamente all’implementazione, Python utilizza la struttura di tipo array per le stringhe: per questo motivo l'indicizzazione e l'estrazione di sottostringhe sono operazioni così semplici e veloci.

## Vedi Anche

Per ulteriori informazioni su come lavorare con le stringhe in Python, dai un'occhiata a questi:

* [Python String Methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
* [Python String Slicing](https://www.pythoncentral.io/cutting-and-slicing-strings-in-python/)
* [Understanding Python's Slice Notation](https://stackoverflow.com/questions/509211/understanding-pythons-slice-notation)