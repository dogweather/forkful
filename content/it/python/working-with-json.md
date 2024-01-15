---
title:                "Lavorare con json"
html_title:           "Python: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Python alla ricerca di un modo semplice e flessibile per gestire dati strutturati, allora lavorare con JSON potrebbe essere esattamente ciò di cui hai bisogno. JSON, o JavaScript Object Notation, è un formato di scambio dati leggero e popolare che è supportato da molti linguaggi di programmazione, inclusi Python.

## Come fare

Per iniziare a utilizzare JSON in Python, devi innanzitutto importare il modulo `json`:

```python
import json
```

Per convertire un oggetto Python in formato JSON, puoi utilizzare il metodo `dumps()` del modulo `json`:

```python
data = { 'nome': 'Mario', 'età': 35, 'lingue': ['italiano', 'inglese', 'francese']}
json_data = json.dumps(data)
print(json_data)
```

Output:
`{"name": "Mario", "age": 35, "languages": ["italian", "english", "french"]}`

Per leggere e interpretare un file JSON, puoi utilizzare il metodo `load()`:

```python
with open("dati.json", "r") as file:
    json_data = json.load(file)
print(json_data)
```

Output:
`{'name': 'Mario', 'age': 35, 'languages': ['italian', 'english', 'french']}`

## Approfondimento

JSON è un formato di dati molto flessibile e può essere utilizzato per rappresentare dati complessi in modo semplice e leggibile. Puoi includere oggetti Python, liste, dizionari e persino numeri e stringhe. Inoltre, JSON è ampiamente utilizzato per la comunicazione tra client e server in applicazioni web e mobile.

Un'ottima risorsa per saperne di più su JSON e sul suo utilizzo in Python è la documentazione ufficiale del modulo `json`: [https://docs.python.org/3/library/json.html](https://docs.python.org/3/library/json.html)

## Vedi anche

- [https://docs.python.org/3/library/json.html](https://docs.python.org/3/library/json.html)
- [https://realpython.com/python-json/](https://realpython.com/python-json/)
- [https://www.datacamp.com/community/tutorials/json-data-python](https://www.datacamp.com/community/tutorials/json-data-python)