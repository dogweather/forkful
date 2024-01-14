---
title:    "Rust: Confrontare due date"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché
Se stai programmando in Rust e hai la necessità di confrontare due date, probabilmente stai lavorando su un'applicazione che richiede la gestione del tempo. Confrontare le date è un'operazione comune nei sistemi informatici, quindi è importante sapere come farlo in modo efficiente in Rust.

## Come fare
In Rust, esistono diverse librerie che offrono funzioni per confrontare le date. In questo articolo, utilizzeremo la libreria built-in "chrono" per eseguire questa operazione. Per iniziare, è necessario importare la libreria nel tuo codice con il seguente codice:

```Rust 
use chrono::{DateTime, Utc};
```

Successivamente, è necessario creare due variabili di tipo "DateTime", una per ogni data che si desidera confrontare. Ad esempio, se vogliamo confrontare la data di oggi con quella di ieri, il codice sarà il seguente:

```Rust
let today = Utc::today();
let yesterday = today - chrono::Duration::days(1);
```

Infine, possiamo utilizzare il metodo "is_before" o "is_after" per confrontare le due date e ottenere un risultato booleano. Ecco un esempio di codice completo:

```Rust
use chrono::{DateTime, Utc};

let today = Utc::today();
let yesterday = today - chrono::Duration::days(1);

if yesterday.is_before(&today) {
    println!("Yesterday is before today");
} else {
    println!("Yesterday is not before today");
}

// Output: Yesterday is before today
```

## Approfondimento
Oltre al metodo "is_before" e "is_after", la libreria "chrono" offre molte altre funzionalità per confrontare le date. Ad esempio, è possibile utilizzare il metodo "with_timezone" per convertire una data in un fuso orario specifico prima di confrontarla con un'altra data. È anche possibile utilizzare il metodo "cmp" per ottenere un valore di confronto, che può essere utilizzato per ordinare le date. Per ulteriori informazioni, consulta la documentazione ufficiale della libreria "chrono".

## Vedi anche
- [Documentazione ufficiale della libreria "chrono"](https://docs.rs/chrono/latest/chrono/)
- [Guida alla programmazione in Rust per principianti](https://www.tutorialspoint.com/rust/index.htm)
- [Esempi di codice Rust su GitHub](https://github.com/rust-lang/rustlings)