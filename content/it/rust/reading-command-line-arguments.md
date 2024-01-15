---
title:                "Lettura degli argomenti da linea di comando"
html_title:           "Rust: Lettura degli argomenti da linea di comando"
simple_title:         "Lettura degli argomenti da linea di comando"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Hai mai avuto la necessità di fornire input al tuo programma direttamente dalla linea di comando? Se sì, allora sei nel posto giusto! In questo articolo, scoprirai come leggere gli argomenti della linea di comando utilizzando il linguaggio di programmazione Rust. Che tu sia un principiante o un esperto, questa abilità ti tornerà utile in molte situazioni.

## Come fare

Per leggere gli argomenti della linea di comando in Rust, è necessario importare il modulo "std::env". Questo ti permetterà di accedere a tutte le funzioni necessarie per gestire gli argomenti della linea di comando. Quindi, è possibile utilizzare la funzione "args()" per ottenere tutti gli argomenti passati alla tua applicazione. Ecco un esempio di codice:

```
use std::env;

fn main(){
    let args: Vec<String> = env::args().collect();
    println!("Gli argomenti della linea di comando sono: {:?}", args);
}
```

Se eseguiamo questo codice fornendo alcuni argomenti, ad esempio "rustc main.rs arg1 arg2", otteniamo il seguente risultato:

```
Gli argomenti della linea di comando sono: ["rustc", "main.rs", "arg1", "arg2"]
```

È importante notare che la funzione "args()" restituisce un vettore (vector) di stringhe. In questo modo, puoi facilmente elaborare e manipolare gli argomenti a tuo piacimento.

## Approfondimento

Oltre alla funzione "args()", esiste anche la funzione "current_exe()" che ti consente di ottenere il percorso del file eseguibile del tuo programma. Questo è particolarmente utile se hai bisogno di gestire i file di configurazione o altri tipi di file correlati al tuo eseguibile.

Inoltre, puoi utilizzare la funzione "var()" per ottenere il valore di una variabile di ambiente specifica. Ad esempio, "var("PATH")" restituirà il percorso di sistema contenuto nella variabile di ambiente PATH.

## Vedi anche

- Documentazione ufficiale di Rust sui moduli: https://doc.rust-lang.org/std
- Articolo su come gestire gli errori in Rust: https://www.linode.com/docs/guides/error-handling-in-rust/
- Risorse per imparare e approfondire il linguaggio Rust: https://github.com/ctjhoa/rust-learning