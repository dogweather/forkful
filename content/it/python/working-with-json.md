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

## Cosa & Perché?
Lavorare con JSON è una pratica comune tra i programmatori, poiché JSON, acronimo di JavaScript Object Notation, è un formato di dati leggibile sia per gli umani che per le macchine. Viene spesso utilizzato per lo scambio di dati tra server e client in applicazioni web.

## Come fare:
Di seguito sono riportati alcuni esempi di codice in Python per lavorare con JSON.

```Python
# Importa il modulo JSON
import json

# Definisci un dizionario
persona = {
    "nome": "Mario",
    "cognome": "Rossi",
    "età": 30
}

# Converti il dizionario in una stringa JSON
persona_json = json.dumps(persona)
print(persona_json)
```
Output:
```
{"nome": "Mario", "cognome": "Rossi", "età": 30}
```

```Python
# Definisci una stringa JSON
libro_json = '{"titolo": "Il Signore degli Anelli", "autore": "J.R.R. Tolkien", "anni_stampa": [1954, 1955]}'

# Converti la stringa JSON in un dizionario
libro = json.loads(libro_json)
print(libro)
```
Output:
```
{'titolo': 'Il Signore degli Anelli', 'autore': 'J.R.R. Tolkien', 'anni_stampa': [1954, 1955]}
```

```Python
# Carica un file JSON esterno
with open("film.json", "r") as file:
    film = json.load(file)

# Mostra il titolo del film
print(film["titolo"])
```
Output:
```
Il Padrino
```

## Approfondimenti:
JSON è stato originariamente sviluppato da Douglas Crockford nel 2001 ed è diventato uno standard di fatto per lo scambio di dati. Sebbene sia molto diffuso, ci sono alternative che vale la pena esplorare, come ad esempio CSV (Comma Separated Values) o XML (eXtensible Markup Language).

Per lavorare con i dati JSON in Python, è possibile utilizzare sia il modulo integrato ```json```, che offre una semplice interfaccia per codificare e decodificare dati, sia il pacchetto esterno ```simplejson```, che offre prestazioni superiori nella gestione di grandi quantità di dati.

## Vedi anche:
- [Documentazione ufficiale di Python su JSON](https://docs.python.org/3/library/json.html)
- [Links a esempi pratici di lavorare con JSON in Python](https://realpython.com/python-json/)