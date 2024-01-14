---
title:    "Gleam: Convertire una stringa in minuscolo"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Perché

La conversione di una stringa in minuscolo è un'operazione comune nella programmazione che può essere utile in diverse situazioni, come ad esempio quando si vuole confrontare o ordinare stringhe alfabeticamente senza tener conto delle maiuscole e minuscole.

# Come fare

Per convertire una stringa in minuscolo in Gleam, possiamo utilizzare la funzione `String.to_lower` seguita dal parametro `string` contenente la stringa che vogliamo convertire. Ad esempio:

```Gleam
let str = "Gleam è un linguaggio di programmazione funzionale"

let lower = String.to_lower(string=str)

io.print(lower)
```

L'output di questo codice sarà: `gleam è un linguaggio di programmazione funzionale`.

# Approfondimenti

Esistono diverse tecniche per la conversione di una stringa in minuscolo, in particolare è importante considerare la codifica utilizzata per la stringa. Se si lavora con stringhe multibyte (come ad esempio quelle contenenti caratteri unicode), è consigliato l'utilizzo della funzione `String.to_lower_utf8` al posto di `String.to_lower`.

È anche importante notare che la funzione `String.to_lower` non modifica la stringa originale, ma crea una nuova stringa in minuscolo. Quindi, se si vuole modificare direttamente la stringa originale, è necessario riassegnare il valore della variabile alla nuova stringa.

# Vedi anche

- Documentazione ufficiale di Gleam sulla funzione `String.to_lower`: <https://gleam.run/stdlib/string/#to_lower>
- Esempi di utilizzo di `String.to_lower` su Gleam Playgrounds: <https://play.gleam.run/?gist=0e9c7ca0f739e97ecf5f5d96e83fe5c1>