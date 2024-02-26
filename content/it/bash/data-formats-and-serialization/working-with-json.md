---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:31.110736-07:00
description: "Lavorare con JSON nella programmazione Bash coinvolge l'analisi, l'estrazione\
  \ e la manipolazione di dati JSON direttamente dalla linea di comando. Gli\u2026"
lastmod: '2024-02-25T18:49:41.482052-07:00'
model: gpt-4-0125-preview
summary: "Lavorare con JSON nella programmazione Bash coinvolge l'analisi, l'estrazione\
  \ e la manipolazione di dati JSON direttamente dalla linea di comando. Gli\u2026"
title: Lavorare con JSON
---

{{< edit_this_page >}}

## Cosa & Perché?
Lavorare con JSON nella programmazione Bash coinvolge l'analisi, l'estrazione e la manipolazione di dati JSON direttamente dalla linea di comando. Gli programmatori spesso fanno ciò per integrare senza problemi gli script shell con le API web e i moderni formati di interscambio dati, rendendo la programmazione in Bash più potente e pertinente in un ecosistema ricco di JSON.

## Come fare:
Bash di per sé non possiede capacità di analisi JSON integrate, ma `jq` è un potente processore JSON da linea di comando che colma questa lacuna. Ecco come utilizzarlo:

**Leggere un file JSON:**

Esempio `data.json`:
```json
{
  "name": "Jane Doe",
  "email": "jane@example.com",
  "location": {
    "city": "New York",
    "country": "USA"
  }
}
```

Per leggere ed estrarre il nome dal file JSON:
```bash
jq '.name' data.json
```
Output:
```
"Jane Doe"
```

**Modificare dati JSON:**

Per aggiornare la città a "Los Angeles" e scrivere di nuovo sul file:
```bash
jq '.location.city = "Los Angeles"' data.json > temp.json && mv temp.json data.json
```

**Analizzare JSON da una variabile:**

Se hai del JSON in una variabile Bash, `jq` può comunque elaborarlo:
```bash
json_string='{"name": "John Doe", "email": "john@example.com"}'
echo $json_string | jq '.name'
```
Output:
```
"John Doe"
```

**Lavorare con array:**

Dato un array di elementi in JSON:
```json
{
  "items": ["apple", "banana", "cherry"]
}
```

Per estrarre il secondo elemento (l'indice parte da 0):
```bash
jq '.items[1]' data.json
```
Output:
```
"banana"
```

Per operazioni più complesse e filtraggi, `jq` ha un manuale completo e tutorial disponibili online, rendendolo uno strumento versatile per tutte le tue necessità Bash/JSON.
