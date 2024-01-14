---
title:                "Rust: Concatenazione di stringhe"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Concatenare stringhe è una delle operazioni più comuni in programmazione, e in Rust ci sono diverse opzioni per farlo in modo efficiente. Sapere come concatenare stringhe correttamente è fondamentale per scrivere codice pulito e performante.

## Come fare

Per concatenare due stringhe in Rust, possiamo utilizzare l'operatore di somma, `+`, che unisce due stringhe in una sola. Ad esempio:

```Rust
let stringa1 = "Ciao, ";
let stringa2 = "mondo!";
let stringa_unificata = stringa1 + stringa2;

println!("{}", stringa_unificata);
```

L'output di questo codice sarà `Ciao, mondo!`, poiché le due stringhe sono state unite in una sola.

Inoltre, Rust offre il metodo `format!()` per formattare e concatenare stringhe in modo più flessibile. Questo metodo accetta degli argomenti e li sostituisce all'interno della stringa. Esempio:

```Rust
let nome = "Paolo";
let cognome = "Rossi";

let saluto = format!("Ciao, {} {}!", nome, cognome);

println!("{}", saluto);
```

L'output sarà sempre `Ciao, Paolo Rossi!`, indipendentemente dai valori delle variabili `nome` e `cognome`.

## Approfondimento

In Rust le stringhe sono immutabili, il che significa che non possono essere modificate una volta create. Tuttavia, questo può essere un problema quando si desidera concatenare un gran numero di stringhe, poiché ogni concatenazione crea una nuova stringa, portando a una potenziale spreco di memoria e prestazioni.

Per evitare questo problema, Rust offre il tipo `String` che è mutabile e permette di effettuare concatenazioni efficienti. Possiamo convertire una stringa in un `String` utilizzando il metodo `to_string()` e successivamente aggiungere altre stringhe con il metodo `push_str()`. Esempio:

```Rust
let mut concatenata = "Ciao".to_string();
concatenata.push_str(", mondo!");

println!("{}", concatenata);
```

L'output sarà lo stesso di prima, ma in questo caso la concatenazione avviene in modo efficiente, senza creare nuove stringhe ogni volta.

## Vedi anche

- [La documentazione ufficiale di Rust sulle stringhe](https://doc.rust-lang.org/std/string/index.html)
- [Un tutorial su come gestire le stringhe in Rust](https://www.callicoder.com/rust-string)
- [Un altro articolo interessante sulle performance delle stringhe in Rust](https://www.lpalmieri.com/posts/2019-09-08-performance-of-strings-in-rust/)