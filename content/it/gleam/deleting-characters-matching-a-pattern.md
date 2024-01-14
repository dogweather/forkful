---
title:                "Gleam: Eliminazione dei caratteri corrispondenti a un pattern"
simple_title:         "Eliminazione dei caratteri corrispondenti a un pattern"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Eliminare caratteri che corrispondono a un determinato modello può essere utile per pulire o manipolare dati in un modo specifico. Questa funzione può essere particolarmente utile quando si lavora con grandi quantità di testo o stringhe.

## Come fare

Per eliminare caratteri che corrispondono a un modello in Gleam, è possibile utilizzare la funzione `String.filter`. Questa funzione prende come input una stringa e una funzione di filtro che determina quali caratteri devono essere rimossi. Esempio:

```Gleam
let pattern = "aeiou"
let string = "hello world"
let filtered_string = String.filter(x -> not String.contains(pattern, x), string)
```

Questo esempio utilizza la funzione `String.contains` per verificare se il carattere corrente è incluso nel modello. Se il carattere non è incluso, viene incluso nella stringa finale. L'output per questo sarebbe `"hll wrld"`. 

## Approfondimento

La funzione `String.filter` può essere combinata con altre funzioni, come ad esempio `String.map` o `String.replace`, per manipolare stringhe in modo ancora più specifico. Inoltre, è possibile utilizzare espressioni regolari per determinare modelli di corrispondenza più complessi. Consulta la documentazione di Gleam per ulteriori informazioni su queste funzioni.

## Vedi anche

- Documentazione ufficiale di Gleam (https://gleam.run/)
- Tutorial su come utilizzare le funzioni di stringa in Gleam (https://dev.to/pragmaticivan/working-with-strings-in-gleam-2jc)
- Un esempio di utilizzo di espressioni regolari in Gleam (https://sivamaksuddulagn.com/posts/pattern-matching-with-regex-in-gleam/)