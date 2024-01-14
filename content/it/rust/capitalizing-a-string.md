---
title:                "Rust: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché 

Capitalizzare una stringa è un'operazione comune quando si lavora con le stringhe in molti linguaggi di programmazione. Ci sono diverse situazioni in cui potresti aver bisogno di capitalizzare una stringa, come ad esempio per rendere uniformi i dati inseriti dall'utente o per confrontare due stringhe in modo case-insensitive.

## Come Fare

Per capitalizzare una stringa in Rust, puoi utilizzare il metodo `to_uppercase` della struttura di dati `String`. Basta passare la stringa che si desidera capitalizzare come argomento e assegnare il risultato a una nuova variabile. Ad esempio:

```Rust
let stringa = String::from("hello world");
let stringa_capitalizzata = stringa.to_uppercase();
println!("{}", stringa_capitalizzata); // output: HELLO WORLD
```

In questo esempio, abbiamo dichiarato una variabile `stringa` che contiene la stringa "hello world" e poi abbiamo chiamato il metodo `to_uppercase` sulla variabile, assegnando il risultato alla variabile `stringa_capitalizzata`. Infine, abbiamo stampato la stringa capitalizzata utilizzando il metodo `println`.

Puoi anche capitalizzare solo la prima lettera di una stringa utilizzando il metodo `capitalize`, che funziona allo stesso modo.

```Rust
let nome = String::from("marco");
let nome_capitalizzato = nome.capitalize();
println!("{}", nome_capitalizzato); // output: Marco
```

## Approfondimento

È importante notare che il metodo `to_uppercase` e `capitalize` non modificano direttamente la stringa originale, ma restituiscono una nuova stringa con la lettera maiuscola o solo la prima lettera in maiuscolo. Ciò significa che è necessario assegnare il risultato del metodo a una nuova variabile o sovrascrivere la stringa originale per utilizzare la versione capitalizzata.

Inoltre, è possibile specificare una lingua specifica come parametro per il metodo `to_uppercase` per garantire che le lettere accentate siano capitalizzate correttamente.

```Rust
let stringa = String::from("ciao");
let stringa_capitalizzata = stringa.to_uppercase(Some(Locale::Language("it".parse().unwrap())))
println!("{}", stringa_capitalizzata); // output: CIAO
```

## Vedi Anche

- [La documentazione ufficiale di Rust per il metodo `to_uppercase`](https://doc.rust-lang.org/std/string/struct.String.html#method.to_uppercase)
- [Ulteriori informazioni su `String` e i suoi metodi](https://doc.rust-lang.org/std/string/struct.String.html)
- [Una guida su come manipolare le stringhe in Rust](https://www.tutorialspoint.com/rust/rust_strings.htm)