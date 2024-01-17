---
title:                "Unire stringhe"
html_title:           "Rust: Unire stringhe"
simple_title:         "Unire stringhe"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

# Rust: Concatenare stringhe

## Cosa & Perché?
Concatenare stringhe significa unire due o più stringhe in un'unica stringa. I programmatori spesso fanno questo per creare un'unica stringa a partire da più parti di testo, ad esempio quando si vuole comporre un messaggio di errore dinamicamente.

## Come fare:
Ecco come si può concatenare stringhe in Rust, utilizzando l'operatore `+`:

```Rust
let str1 = "Ciao";
let str2 = "mondo!";
let str3 = str1 + str2;
println!("{}", str3); // Output: Ciaomondo!
```

E se invece volessimo separare le stringhe con uno spazio, possiamo farlo in questo modo:

```Rust
let str1 = "Ciao";
let str2 = "mondo!";
let str3 = format!("{} {}", str1, str2);
println!("{}", str3); // Output: Ciao mondo!
```

## Approfondimento:
La concatenazione di stringhe è stata introdotta nella versione 1.38 di Rust, insieme all'operatore `+` che permette di unire le stringhe più facilmente rispetto all'utilizzo del metodo `.to_string()`.

Un'alternativa alla concatenazione di stringhe in Rust è utilizzare il tipo `String` che può essere modificato in modo dinamico aggiungendo testo con il metodo `.push_str()`.

L'implementazione della concatenazione di stringhe in Rust è ottimizzata per prestazioni, in modo da utilizzare il minor numero di allocazioni di memoria possibile.

## Vedi anche:
- [La documentazione ufficiale di Rust su `String`](https://doc.rust-lang.org/std/string/struct.String.html)
- [Un video tutorial sulla concatenazione di stringhe in Rust](https://www.youtube.com/watch?v=b8WH_xKdI2U)
- [Un esempio pratico di concatenazione di stringhe in Rust](https://play.integer32.com/?gist=90f1d1d1a255cda4b2de833742f8d423&version=stable&mode=release&edition=2018)