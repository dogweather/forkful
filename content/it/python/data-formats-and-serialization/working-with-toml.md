---
title:                "Lavorare con TOML"
aliases:
- /it/python/working-with-toml/
date:                  2024-01-26T04:25:23.567870-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/working-with-toml.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
TOML, acronimo di Tom's Obvious, Minimal Language, è un formato di serializzazione dei dati simile a JSON o YAML, ma mira alla semplicità e leggibilità. I programmatori utilizzano TOML per i file di configurazione perché è facile da scrivere e comprendere, e si mappa ordinatamente sulle strutture dati nei linguaggi di programmazione come Python.

## Come fare:
Prima di iniziare, installare il pacchetto `toml` con `pip install toml`. Vediamo come analizzare un file TOML:

```python
import toml

# Esempio di contenuto TOML come stringa
toml_string = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z # Date di prima classe

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
"""

# Analizzare la stringa TOML
parsed_toml = toml.loads(toml_string)

# Accesso ai dati
print(parsed_toml['owner']['name'])  # Output: Tom Preston-Werner
print(parsed_toml['database']['ports'])  # Output: [8001, 8001, 8002]
```

## Approfondimento
TOML è stato creato da Tom Preston-Werner, uno dei fondatori di GitHub, come formato di file di configurazione più user-friendly. È progettato per mappare in modo univoco su una tabella hash ed essere facilmente analizzabile dalle macchine.

Rispetto a JSON, TOML è più leggibile per i file di configurazione e supporta i commenti. YAML, un'altra alternativa, può essere più compatto, ma la sua dipendenza dall'indentazione e questioni sottili, come l'inesistenza dei tab, possono confondere le persone.

Per quanto riguarda i dettagli di implementazione, i valori TOML sono tipizzati, il che include stringhe, interi, float, booleani, datetime, array e tabelle. Tutto è case-sensitive. Inoltre, TOML supporta stringhe su più linee e, a partire dall'ultima versione, consente anche array di tipi eterogenei.

Python utilizza la libreria `toml`, che rispecchia le librerie JSON e YAML in termini di API. Si hanno `toml.load` e `toml.loads` per leggere TOML da un file o una stringa, rispettivamente, e `toml.dump` e `toml.dumps` per scriverlo.

## Vedi Anche
- Il repository GitHub ufficiale di TOML per le specifiche: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- La documentazione della libreria Python `toml`: [pypi.org/project/toml/](https://pypi.org/project/toml/)
- Esempi concreti di TOML: File di configurazione per il gestore dei pacchetti di Rust `cargo` o lo strumento di confezionamento Python `poetry`.
