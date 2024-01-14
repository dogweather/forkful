---
title:    "Rust: Convertire una stringa in minuscolo"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché
Se stai scrivendo un programma in Rust e hai bisogno di manipolare delle stringhe, potresti trovarvi nella situazione in cui hai bisogno di convertire una stringa in minuscolo. Ci sono diverse ragioni per cui potresti aver bisogno di farlo: forse devi confrontare due stringhe per controllare se sono identiche, o forse vuoi semplicemente rendere il tuo output più uniforme e leggibile. In ogni caso, è utile sapere come eseguire questa operazione in Rust.

## Come fare
Per convertire una stringa in minuscolo in Rust, puoi utilizzare il metodo `to_lowercase()`. Questo metodo prende in input una stringa e restituisce una nuova stringa con tutti i caratteri in minuscolo. Ad esempio:

```Rust
let stringa = "Ciao Mondo";
let stringa_minuscola = stringa.to_lowercase();

println!("{}", stringa_minuscola);

// Output: ciao mondo
```

Come puoi vedere dall'esempio sopra, semplicemente chiamando il metodo `to_lowercase()` sulla tua stringa, il risultato verrà restituito in minuscolo.

## Approfondimento
Se desideri saperne di più sul processo di conversione delle stringhe in minuscolo in Rust, puoi esplorare il funzionamento interno del metodo `to_lowercase()`. Quando questo metodo viene chiamato, viene creata una nuova stringa vuota che viene quindi popolata da un ciclo che itera su ogni carattere della stringa originale. Ogni carattere viene convertito in minuscolo utilizzando la funzione `to_ascii_lowercase()` e quindi aggiunto alla nuova stringa. Infine, la nuova stringa viene restituita come output.

È importante notare che questo metodo utilizza il sistema di codifica ASCII per convertire i caratteri in minuscolo. Ciò significa che i caratteri speciali o di accenti potrebbero non essere correttamente convertiti. Per gestire questi casi, è possibile utilizzare la libreria di terze parti `unicode-segmentation` che supporta diverse codifiche, ma questo va al di là dello scopo di questo articolo.

## Vedi anche
- [Documentazione Rust su stringhe](https://doc.rust-lang.org/std/string/)
- [Libreria Unicode-segmentation per gestire diverse codifiche](https://crates.io/crates/unicode-segmentation)
- [Funzioni di manipolazione delle stringhe in Rust](https://www.rust-lang.org/it/learn/strings)