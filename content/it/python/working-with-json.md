---
title:                "Python: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

La manipolazione di dati in formato JSON è una parte essenziale della programmazione in Python moderna, in quanto questo formato è ampiamente utilizzato per lo scambio di dati tra applicazioni web e backend. Imparare a lavorare con JSON può aiutare a migliorare le tue capacità di programmazione e aprirti nuove opportunità per lo sviluppo di applicazioni.

## Come fare

Per iniziare, importa il modulo `json` nella tua applicazione Python:

```
import json
```

Per convertire un oggetto Python in una stringa JSON, puoi utilizzare il metodo `dumps()` del modulo `json`. Ad esempio, se hai un dizionario di dati:

```
dizionario = {"nome": "Giovanni", "cognome": "Rossi", "età": 35}
```

Puoi utilizzare il seguente codice per convertire il dizionario in una stringa JSON:

```
stringa_json = json.dumps(dizionario)
```

Per salvare questa stringa in un file, puoi usare la funzione `dump()` del modulo `json`:

```
with open("dati.json", "w") as file:
    json.dump(stringa_json, file)
```

Per leggere un file JSON e convertirlo in un oggetto Python, puoi utilizzare il metodo `load()` del modulo `json`:

```
with open("dati.json", "r") as file:
    dati = json.load(file)
```

## Approfondimento

Oltre alle funzioni di base per la conversione di dati da e verso il formato JSON, il modulo `json` fornisce anche metodi avanzati per manipolare tali dati. Ad esempio, puoi utilizzare il metodo `loads()` per convertire una stringa JSON in un oggetto Python, e il metodo `pretty()` per formattare una stringa JSON in modo leggibile dagli umani.

Inoltre, il modulo `json` include funzionalità per la gestione di errori durante la conversione dei dati, come ad esempio la gestione di tipi di dati non supportati o conversioni non valide.

## Vedi anche

- [Documentazione ufficiale del modulo JSON in Python](https://docs.python.org/3/library/json.html)
- [Tutorial sul formato JSON di Real Python](https://realpython.com/python-json/)
- [Tutorial sulla manipolazione di dati JSON in Python di GeeksforGeeks](https://www.geeksforgeeks.org/python-working-with-json-data/)