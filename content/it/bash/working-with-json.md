---
title:                "Lavorare con JSON"
date:                  2024-01-19
html_title:           "Arduino: Lavorare con JSON"
simple_title:         "Lavorare con JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON è un formato di dati leggero per lo scambio di informazioni. I programmatori lo usano per la sua semplicità e per la facilità di integrazione con diverse lingue di programmazione.

## How to:
Per lavorare con JSON in Bash, possiamo usare `jq`, uno strumento da riga di comando per l'elaborazione di JSON. Di seguito alcuni esempi:

```Bash
# Installa jq
sudo apt-get install jq

# Leggi un valore da un oggetto JSON
echo '{"nome": "Mario", "cognome": "Rossi"}' | jq '.nome'

# Risultato
"Mario"

# Filtra un array JSON
echo '[{"nome": "Mario"}, {"nome": "Luigi"}]' | jq '.[] | select(.nome=="Mario")'

# Risultato
{
  "nome": "Mario"
}
```

## Deep Dive
JSON, JavaScript Object Notation, è nato negli anni 2000 e si è rapidamente affermato come standard per lo scambio di dati su Internet. Alternativamente, si possono utilizzare XML o YAML, ma JSON prevale per la sua leggibilità e convenienza. Basta ricordarsi che Bash non ha un supporto integrato per JSON, quindi strumenti come `jq` sono essenziali.

## See Also
- Documentazione ufficiale di jq: https://stedolan.github.io/jq/manual/
- Tutorial JSON: https://www.w3schools.com/js/js_json_intro.asp
- Confronto tra JSON, XML, e YAML: https://www.json.org/json-it.html
