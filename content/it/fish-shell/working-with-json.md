---
title:                "Lavorare con JSON"
date:                  2024-01-19
html_title:           "Arduino: Lavorare con JSON"
simple_title:         "Lavorare con JSON"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?

Lavorare con JSON significa manipolare un formato di scambio dati leggero e facilmente leggibile. Gli sviluppatori lo fanno per interagire con API, configurazioni, e per lo scambio dati tra front-end e back-end.

## Come Fare:

```Fish Shell
# Installa jq per manipolare JSON
fish> sudo apt install jq

# Formattare e visualizzare JSON
fish> echo '{"nome": "Mario", "città": "Roma"}' | jq .
{
  "nome": "Mario",
  "città": "Roma"
}

# Estrai un campo specifico
fish> echo '{"nome": "Mario", "città": "Roma"}' | jq '.nome'
"Mario"
```

## Approfondimento:

Il JSON, acronimo di JavaScript Object Notation, è nato dall'JavaScript ma ora è indipendente da qualsiasi linguaggio. Alternativamente si potrebbe usare XML, ma JSON è più snello e meglio integrato con le tecnologie web moderne. La manipolazione diretta di JSON in Fish senza strumenti esterni non è ideale; strumenti come `jq` semplificano il processo, offrendo un parsing robusto e funzionalità di ricerca.

## Vedi Anche:

- La documentazione di `jq`: https://stedolan.github.io/jq/manual/
- JSON su MDN Web Docs: https://developer.mozilla.org/it/docs/Learn/JavaScript/Objects/JSON
- Introduzione ai formati di dati web su W3Schools: https://www.w3schools.com/js/js_json_intro.asp
