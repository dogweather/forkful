---
title:    "Rust: Scrivere test"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Perché scrivere test è importante in Rust

Scrivere test è fondamentale per garantire che il codice che scriviamo funzioni correttamente e senza errori. In Rust, questa pratica è ancora più importante poiché il linguaggio è noto per la sua sicurezza e affidabilità. Scrivere test ci aiuta a individuare e correggere eventuali bug prima che possano causare problemi nel nostro programma.

## Come scrivere test in Rust

Per scrivere test in Rust, possiamo utilizzare il framework incorporato di test di Rust. Questo framework ci consente di scrivere e eseguire test unitari e di integrazione in modo semplice ed efficiente. Vediamo un esempio di come scrivere un test unitario per una funzione di calcolo del fattoriale:

```Rust
// Importiamo il modulo dei test
mod tests {
    // Utilizziamo l'attributo `test` per indicare che questa è una funzione di test
    #[test]
    // Definiamo una funzione di test che verifica se il risultato della nostra funzione è corretto
    fn test_fattoriale() {
        // Chiamiamo la nostra funzione con un input noto
        let result = calcolo_fattoriale(5);
        // Utilizziamo il macro `assert_eq` per verificare se il risultato è uguale a quello atteso
        assert_eq!(result, 120);
    }
}

// Definiamo la nostra funzione di calcolo del fattoriale
fn calcolo_fattoriale(n: u32) -> u32 {
    if n == 0 {
        1
    } else {
        n * calcolo_fattoriale(n - 1)
    }
}
```

Una volta scritto il nostro test, possiamo eseguirlo utilizzando il comando `cargo test`. Se il test passa, vedremo un output positivo, altrimenti verrà segnalato un fallimento con un messaggio descrittivo.

## Approfondimento sui test in Rust

In aggiunta ai test unitari, possiamo scrivere anche test di integrazione per verificare il funzionamento corretto di più componenti del nostro programma. Inoltre, Rust ci offre anche la possibilità di scrivere test di proprietà, ovvero test che verificano che determinate proprietà del nostro codice siano rispettate.

In generale, è una buona pratica includere test nella nostra pipeline di sviluppo, eseguendoli automaticamente ogni volta che facciamo una modifica al codice. In questo modo possiamo individuare e risolvere bug in modo più efficiente e avere una maggiore fiducia nel nostro codice.

## Vedi anche

- [Documentazione di test in Rust](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Esempio di test in Rust su GitHub](https://github.com/rust-lang/book/blob/master/src/fattoriale/tests/fattoriale.rs)
- [Rustlings](https://github.com/rust-lang/rustlings) - Esercizi di Rust che includono anche la scrittura di test.