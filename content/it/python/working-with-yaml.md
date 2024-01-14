---
title:                "Python: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché
Il formato YAML, acronimo di "YAML Ain't Markup Language", è un linguaggio di markup leggibile per l'uomo, che viene utilizzato principalmente per rappresentare dati in modo strutturato. È molto utile per chi lavora con programmi che richiedono input strutturati, come ad esempio l'automazione di processi o la gestione di configurazioni.

## Come Funziona
Per iniziare ad utilizzare YAML all'interno del tuo codice Python, è necessario installare il pacchetto PyYAML utilizzando il comando `pip install pyyaml`. Una volta installato, è possibile importare il modulo all'interno del tuo script utilizzando `import yaml`.

Per creare un nuovo file YAML, si può semplicemente scrivere il seguente codice all'interno di un blocco di codice "```Python" e salvarlo con l'estensione .yaml:

```
import yaml
my_data = {'nome': 'Maria', 'cognome': 'Rossi', 'eta': 30}
with open("dati.yaml", "w") as f:
    yaml.dump(my_data, f)
```
L'output sarà un file YAML che rappresenta i dati definiti nel nostro script:

```
nome: Maria
cognome: Rossi
eta: 30
```

Per leggere un file YAML e accedere ai dati al suo interno, si può utilizzare il seguente codice:

```
with open("dati.yaml", "r") as f:
    data = yaml.load(f)
print(data)
```

L'output sarà un dizionario Python con le chiavi e i valori definiti nel file YAML:

```
{'nome': 'Maria', 'cognome': 'Rossi', 'eta': 30}
```

## Approfondimento 
Il formato YAML è strutturato in modo molto semplice, con l'uso di indentazione per definire la struttura dei dati. È possibile utilizzare liste, dizionari e tipi di dati scalar all'interno di un file YAML.

Una delle principali caratteristiche di YAML è la sua leggibilità per l'uomo, il che lo rende molto utile anche per la gestione di file di configurazione. Inoltre, è possibile utilizzare commenti all'interno dei file YAML per aggiungere note o informazioni aggiuntive.

Un'altra caratteristica utile di YAML è la possibilità di utilizzare referenze ai dati già definiti all'interno del file. Questo rende più semplice la gestione di file YAML di grandi dimensioni e complessi.

## Vedi Anche
- [Documentazione ufficiale di PyYAML](https://pyyaml.org/wiki/PyYAMLDocumentation)
- [Tutorial su come utilizzare YAML in Python](https://realpython.com/python-yaml/)
- [Esempi di file YAML](https://datahub.io/collections/yaml-examples)