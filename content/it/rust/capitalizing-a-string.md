---
title:                "Rust: Capitalizzare una stringa"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Se sei nuovo alla programmazione in Rust, potresti chiederti perché qualcuno vorrebbe capitalizzare una stringa. In realtà, ci sono molte situazioni in cui il capitalizzare una stringa può essere molto utile. Ad esempio, potresti voler visualizzare correttamente i nomi dei tuoi utenti o creare una stringa con una parola in maiuscolo come titolo. Imparare a capitalizzare una stringa è un'abilità importante per ogni programmatore Rust.

## Come fare

In Rust, puoi capitalizzare una stringa in modo molto semplice utilizzando il metodo `to_uppercase()`. In questo metodo, basta passare la tua stringa come argomento e otterrai una nuova stringa con tutti i caratteri convertiti in maiuscolo.

```Rust
let stringa = "questa è una stringa da capitalizzare";
let stringa_in_maiuscolo = stringa.to_uppercase();

println!("{}", stringa_in_maiuscolo); // OUTPUT: QUESTA È UNA STRINGA DA CAPITALIZZARE
```

Se preferisci capitalizzare solo la prima lettera di una stringa, puoi utilizzare il metodo `capitalize()`.

```Rust
let stringa = "questa è una stringa da capitalizzare";
let stringa_con_prima_lettera_in_maiuscolo = stringa.to_ascii_lowercase().capitalize();

println!("{}", stringa_con_prima_lettera_in_maiuscolo); // OUTPUT: Questa è una stringa da capitalizzare
```

## Approfondimento

Se vuoi capire meglio come funziona il metodo `to_uppercase()` e imparare a capitalizzare una stringa manualmente, puoi approfondire il concetto di Unicode e di codifica dei caratteri in Rust. In poche parole, il metodo `to_uppercase()` utilizza tabelle di conversione per trasformare i caratteri in maiuscolo. Invece di utilizzare questo metodo predefinito, puoi utilizzare queste tabelle di conversione per creare una funzione personalizzata che adatta la conversione ai tuoi specifici requisiti.

## Vedi anche

- [Unità e conversioni di stringhe in Rust](https://doc.rust-lang.org/std/ascii/struct.EscapeDefault.html#example)
- [Rust Unicode FAQ](https://www.rust-lang.org/it-IT/frequently-asked-questions#unicode)