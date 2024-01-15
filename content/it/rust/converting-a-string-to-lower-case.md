---
title:                "Convertire una stringa in minuscolo"
html_title:           "Rust: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché
Convertire una stringa in minuscolo è un'operazione comune nel processo di elaborazione dei dati in Rust. Può essere utile per confrontare le stringhe in modo uniforme, evitando errori dovuti alle lettere maiuscole e minuscole.

## Come Fare
Per convertire una stringa in minuscolo in Rust, possiamo utilizzare il metodo `to_lowercase()` della struttura `String` come mostrato nell'esempio seguente:

```Rust
let stringa = "Ciao Mondo!";
let nuova_stringa = stringa.to_lowercase();

println!("{}", nuova_stringa); //Output: ciao mondo!
```

Il metodo `to_lowercase()` restituirà una nuova stringa in minuscolo, senza modificare quella originale. Possiamo anche utilizzare il metodo `to_lowercase()` su una referenza mutabile alla stringa originale per modificarla direttamente, come nel seguente esempio:

```Rust
let mut stringa = "Ciao Mondo!";
stringa.to_lowercase(); //Modifica direttamente la stringa originale

println!("{}", stringa); //Output: ciao mondo!
```

Se necessario, possiamo anche utilizzare il metodo `into_lowercase()` della struttura `String` per convertire una stringa in minuscolo e restituirla come un `String` in una sola riga di codice.

```Rust
let stringa = "Ciao Mondo!".to_string();
let nuova_stringa = stringa.into_lowercase();

println!("{}", nuova_stringa); //Output: ciao mondo!
```

## Deep Dive
La conversione di una stringa in minuscolo può sembrare un'operazione semplice, ma ci sono alcune complicazioni da considerare. Ad esempio, la conversione in minuscolo non è una semplice sostituzione delle lettere maiuscole con le minuscole corrispondenti. Alcune lettere hanno più corrispondenti in minuscolo, come la lettera 'ß' in tedesco che può essere sostituita con 'ss' o 'ẞ' a seconda del contesto. Inoltre, alcune lingue hanno caratteri che non hanno una corrispondenza in minuscolo. Pertanto, quando si lavora con stringhe multilingue, è importante tenere conto delle differenze culturali nella conversione in minuscolo.

## See Also
- [The Rust Programming Language | Rust Documentation](https://doc.rust-lang.org/book/)
- [Convert a String to Lowercase in Rust | GeeksforGeeks](https://www.geeksforgeeks.org/convert-a-string-to-lowercase-in-rust/)