---
title:                "Gleam: Capitalizzazione di una stringa"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalize una stringa è un'operazione comune quando si lavora con dati di testo in un programma. Potresti aver bisogno di capitalizzare un nome o una parola in maiuscolo per soddisfare determinate esigenze di formattazione o input.

## Come fare

Per capitalizzare una stringa in Gleam, puoi utilizzare la funzione `String.to_uppercase()` come mostrato nell'esempio seguente:

```Gleam
let name = "giovanni"

let capitalized_name = String.to_uppercase(name)

io.println(capitalized_name) // Output: GIOVANNI
```

Puoi anche controllare se una stringa è già in maiuscolo utilizzando la funzione `String.is_uppercase()` e, se necessario, utilizzare la funzione `String.to_lowercase()` per convertirla in minuscolo.

## Approfondimento

Capitalizzare una stringa può sembrare una semplice operazione, ma in realtà ci sono alcune considerazioni da tenere presente.

In Gleam, le stringhe sono immutabili, il che significa che quando vengono trasformate, viene creata una nuova stringa invece di modificare quella esistente. Questo può comportare un aumento di memoria e performance in caso di lavori su stringhe molto grandi e frequenti trasformazioni.

Inoltre, le funzioni di capitalizzazione nel linguaggio dipendono dalle specifiche di localizzazione del sistema operativo, quindi è importante tenere conto di possibili differenze nei risultati.

## Vedi anche

- Documentazione ufficiale di Gleam sulle stringhe: https://gleam.run/documentation/standard-library/strings/
- Un tutorial su come lavorare con stringhe in Gleam: https://dev.to/gleam_lang/writing-text-programs-with-gleam-40ch
- Una discussione sulla differenza tra la funzioni di capitalizzazione `to_uppercase()` e `to_titlecase()` in Gleam: https://github.com/gleam-lang/gleam/issues/364