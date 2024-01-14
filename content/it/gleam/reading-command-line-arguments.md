---
title:                "Gleam: Lettura degli argomenti della riga di comando"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché Leggere Gli Argomenti Della Riga Di Comando in Gleam

Se sei un programmatore Gleam novizio o stai cercando di espandere le tue conoscenze nel linguaggio, potresti chiederti perché leggere gli argomenti della riga di comando potrebbe essere importante. Leggere gli argomenti della riga di comando è un'abilità fondamentale che ti consentirà di creare programmi interattivi e personalizzati per gli utenti. Continua a leggere per scoprire come farlo con Gleam!

## Come Leggere gli Argomenti della Riga di Comando in Gleam

Per leggere gli argomenti della riga di comando in Gleam, utilizzare il modulo `gleam/arg"` e la sua funzione `parse/1`. Questa funzione accetta una lista di argomenti `String` da leggere e restituisce una `struct` contenente i valori di ogni argomento.

```Gleam
import gleam/arg

let arg_list = ["--name", "John", "--age", "25"]
let args = arg.parse(arg_list)

// Accedere agli argomenti
let name = args["name"]
let age = args["age"]

// Stampare l'output
io.format("Ciao {}, quest'anno compirai {} anni!\n", [name, age])
```

L'output di questo codice sarà "Ciao John, quest'anno compirai 25 anni!". Nota che gli argomenti devono essere preceduti da `"--"` e che gli argomenti possono essere passati in qualsiasi ordine.

## Approfondimenti su Come Leggere Gli Argomenti della Riga di Comando

Oltre alla funzione `parse/1`, il modulo `gleam/arg` offre altre funzioni utili per leggere gli argomenti della riga di comando. Ad esempio, la funzione `parse_or_exit/1` non solo restituirà gli argomenti, ma interromperà anche il programma e restituirà un messaggio di errore se gli argomenti forniti non sono validi. Inoltre, gli argomenti possono essere passati anche come argomenti posizionali invece che come opzioni nominate.

## Vedi Anche

- Documentazione ufficiale sul modulo `gleam/arg`: https://gleam.run/modules/gleam/arg.html
- Tutorial su come leggere la riga di comando in Gleam: https://dev.to/gleam_lang/learning-gleam-parsing-command-line-arguments-1mm5
- Approfondimenti su come utilizzare gli argomenti posizionali nella riga di comando: https://dev.to/gleam_lang/learning-gleam-parsing-arguments-29bl