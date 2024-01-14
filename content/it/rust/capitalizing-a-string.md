---
title:    "Rust: Maiuscolizzare una stringa"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché capitilizzare una stringa

Capitalizzare una stringa è un'operazione comune durante la scrittura di codice, specialmente quando si lavora con testo. In Rust, ci sono alcune diverse opzioni per eseguire questa operazione, ognuna con i suoi vantaggi e svantaggi. In questo articolo, esploreremo perché e come capitalizzare una stringa in Rust.

## Come farlo

Per prima cosa, dobbiamo importare il modulo `std::string::String`, perché è in questo modulo che si trovano le funzioni per manipolare le stringhe in Rust.

```Rust
use std::string::String;
```

Una delle opzioni più semplici è utilizzare il metodo `to_uppercase()` su una stringa già esistente. Questo metodo restituirà una nuova stringa con tutte le lettere convertite in maiuscolo.

```Rust
let stringa = "ciao";
let stringa_uppercase = stringa.to_uppercase();
println!("{}", stringa_uppercase);
```

Questo codice produrrà l'output `"CIAO"`.

Un'altra opzione è utilizzare il metodo `replace()` su una stringa esistente per sostituire ogni lettera con la versione maiuscola.

```Rust
let stringa = "ciao";
let stringa_uppercase = stringa.replace(|c| c.to_uppercase(), "");
println!("{}", stringa_uppercase);
```

Questo codice produrrà lo stesso output `"CIAO"`.

Infine, possiamo utilizzare il metodo `chars()` per scorrere attraverso ogni carattere della stringa e utilizzare il metodo `char::to_uppercase()` per convertire ogni carattere in maiuscolo.

```Rust
let stringa = "ciao";
let mut stringa_uppercase = String::new();
for c in stringa.chars() {
    stringa_uppercase.push(c.to_uppercase());
}
println!("{}", stringa_uppercase);
```

Anche questo codice produrrà l'output `"CIAO"`.

## Approfondimento

Sebbene sia semplice capitalizzare una stringa in Rust, ci sono alcune cose da tenere a mente. Per esempio, è importante ricordare che le stringhe in Rust sono immutabili per impostazione predefinita, quindi verrà restituita una nuova stringa capitalizzata, ma la stringa originale rimarrà invariata. Inoltre, se si sta lavorando con stringhe UTF-8, è importante gestire correttamente i caratteri non ASCII durante la conversione in maiuscolo.

Se vuoi approfondire l'argomento della manipolazione delle stringhe in Rust, puoi consultare la documentazione ufficiale su [rust-lang.org](https://doc.rust-lang.org/std/string/). Inoltre, puoi trovare una guida dettagliata sulla gestione dei caratteri non ASCII in Rust [qui](https://stackoverflow.com/questions/31649887/utf-8-string-manipulation-in-rust).

## Vedi anche

- [The Rust Programming Language - Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Rust Standard Library - String](https://doc.rust-lang.org/std/string/index.html)
- [Rust By Example - Strings](https://doc.rust-lang.org/stable/rust-by-example/std/string.html)