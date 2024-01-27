---
title:                "Lavorare con JSON"
date:                  2024-01-19
html_title:           "Arduino: Lavorare con JSON"
simple_title:         "Lavorare con JSON"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Lavorare con i JSON in Python significa gestire dati strutturati come testo. I programmatori li usano per scambi dati in modo leggero tra server e app web.

## How to:
```Python
# Importa json
import json

# Dizionario di esempio
persona = {"nome": "Giovanni", "età": 30, "città": "Roma"}

# Converti in JSON
persona_json = json.dumps(persona)
print(persona_json)  # Output: {"nome": "Giovanni", "età": 30, "città": "Roma"}

# Converti stringa JSON in dizionario
persona_dizionario = json.loads(persona_json)
print(persona_dizionario)  # Output: {'nome': 'Giovanni', 'età': 30, 'città': 'Roma'}

# Salvare un JSON in un file
with open('persona.json', 'w') as file:
    json.dump(persona, file)

# Leggere un JSON da un file
with open('persona.json', 'r') as file:
    data_letta = json.load(file)
print(data_letta)  # Output: {'nome': 'Giovanni', 'età': 30, 'città': 'Roma'}
```

## Deep Dive
Il JSON (JavaScript Object Notation) è nato nei primi anni 2000 come alternativa semplice a XML. Altro formato: YAML, più leggibile ma meno diffuso. In Python, il modulo `json` trasforma dati Python in JSON e viceversa. Hanno rappresentazioni simili ma attenzione: JSON non accetta commenti e le chiavi sono sempre in doppie virgolette.

## See Also
Per sapere di più:
- Documentazione ufficiale del modulo `json`: https://docs.python.org/3/library/json.html
- JSON vs. XML: https://www.w3schools.com/js/js_json_xml.asp
- Intro a YAML (in inglese): https://yaml.org/start.html
