---
title:                "Gleam: Eliminazione di caratteri corrispondenti ad un modello"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Spesso durante la scrittura di codice, potresti aver bisogno di eliminare alcuni caratteri particolari da una stringa. Puoi farlo manualmente, ma non sarebbe bello avere una funzione che possa farlo per te?

## Come fare

Ecco dove entra in gioco *delete_matching*. Questa funzione, disponibile in Gleam, consente di eliminare tutti i caratteri che corrispondono a un determinato modello all'interno di una stringa.

```Gleam
let nome = "Giacomo-123!"
let risultato = delete_matching(nome, "-123")
gleam_assert.equal(risultato, "Giacomo!")
```

In questo esempio, stiamo eliminando i caratteri "-123" dalla nostra stringa di input "Giacomo-123!". Vediamo il risultato della funzione nella variabile "risultato".

## Approfondimento

Ma come funziona esattamente *delete_matching*? In realtà, questa funzione è solo una casella di caramelle che contiene altri strumenti come regex e split. In sostanza, questa è una combinazione di una regex che individua il modello che desideriamo eliminare e una split che divide la stringa in due parti, mantenendo solo la parte desiderata.

Vediamo un esempio di questo processo dietro le quinte:

```Gleam
let nome = "Giacomo-123!"
let pattern = "-123"
let regex = Regex.new(pattern)
let parts = String.split(nome, regex)
gleam_assert.equal(parts, ["Giacomo", ""])
```

Come puoi vedere, il nostro stringa è stata divisa in due parti: "Giacomo" e "-123!". Infine, possiamo unire nuovamente solo la prima parte, ottenendo il risultato finale.

## Vedi anche

- Documentazione ufficiale di Gleam su *delete_matching*
- Esempi di utilizzo della funzione *delete_matching* in altri progetti Gleam
- Ulteriori informazioni sulle funzionalità di regexp disponibili in Gleam