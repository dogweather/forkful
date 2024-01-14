---
title:                "Rust: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Se sei nuovo al mondo della programmazione, potresti chiederti perché dovresti imparare a concatenare delle stringhe. In termini semplici, la concatenazione di stringhe è un'operazione fondamentale nella creazione di applicazioni. È essenziale per combinare diverse parole o numeri in una stringa più grande, rendendo il codice più dinamico e flessibile.

## Come Fare

Per concatenare le stringhe in Rust, è necessario utilizzare il simbolo "plus" (+). Vediamo un esempio:

```Rust
let stringa_1 = "Ciao";
let stringa_2 = "Mondo";
let stringa_finale = stringa_1 + " " + stringa_2;
```

In questo codice, abbiamo creato due stringhe e poi le abbiamo concatenate insieme utilizzando il simbolo "+". Nota che abbiamo inserito anche uno spazio vuoto tra le due stringhe per assicurarci che la stringa finale sia formattata correttamente.

Puoi anche concatenare più di due stringhe contemporaneamente. Ad esempio:

```Rust
let stringa_1 = "Benvenuto";
let stringa_2 = "in";
let stringa_3 = "Rust";
let stringa_finale = stringa_1 + " " + stringa_2 + " " + stringa_3;
```

Questo codice produrrà la stringa finale "Benvenuto in Rust".

## Approfondimento

In Rust, la concatenazione di stringhe è ancora più efficiente grazie all'uso dei tipi di dati "String" e "StringBuf". Questi tipi di dati consentono di concatenare stringhe senza dover ricreare ogni volta nuove copie delle stringhe originali. Inoltre, Rust offre anche il metodo "format!" per concatenare le stringhe in modo più efficiente.

Se vuoi approfondire ulteriormente questo argomento, puoi leggere la documentazione ufficiale di Rust su [concatenazione di stringhe](https://doc.rust-lang.org/std/string/) e [uso del metodo format!](https://doc.rust-lang.org/std/macro.format.html)

## Vedi Anche

* [Documentazione Ufficiale di Rust](https://www.rust-lang.org/it/learn)
* [Rust per principianti: Cos'è Rust e perché dovresti impararlo](https://www.davideaversa.it/2018/10/rust-per-principianti/) 
* [Video Tutorial: Concatenazione di Stringhe in Rust](https://www.youtube.com/watch?v=MvptA2A7hnE)