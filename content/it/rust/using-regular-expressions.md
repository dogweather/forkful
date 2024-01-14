---
title:    "Rust: Utilizzare le espressioni regolari"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Le espressioni regolari sono uno strumento potente per la manipolazione e validazione di stringhe di testo. Utilizzando Rust, è possibile utilizzare le espressioni regolari per trovare pattern specifici all'interno di una stringa, sostituire parti di testo e molto altro ancora.

## Come fare

Per utilizzare le espressioni regolari in Rust, è necessario importare il modulo `regex` e creare un oggetto `Regex` con il pattern desiderato:

```Rust
use regex::Regex;

let re = Regex::new(r"ab+c").unwrap();
```

Successivamente, è possibile utilizzare il metodo `find` per trovare una corrispondenza nella stringa di input. Il metodo `find` restituisce un oggetto `Option<Match>` che può essere gestito utilizzando il metodo `unwrap`:

```Rust
let text = "abbcccd";

let result = re.find(text).unwrap();

println!("La prima corrispondenza di `ab+c` in `{}` si trova tra gli indirizzi {} e {}.",
     text, result.start(), result.end());
```

L'output sarebbe:

```
La prima corrispondenza di `ab+c` in `abbcccd` si trova tra gli indirizzi 0 e 4.
```

## Approfondimento

È possibile utilizzare le espressioni regolari in Rust per effettuare ricerche e sostituzioni, effettuare validazioni di input e molto altro ancora. Inoltre, è possibile utilizzare i cosiddetti "gruppi" per effettuare sostituzioni basate sulla corrispondenza di porzioni specifiche della stringa di input.

Per una spiegazione più dettagliata sull'utilizzo delle espressioni regolari in Rust, ti consiglio di consultare la documentazione ufficiale sul modulo `regex` o cercare tutorial online.

## Vedi anche

- Documentazione ufficiale sul modulo `regex`: https://docs.rs/regex/1.4.2/regex/
- Tutorial sull'utilizzo delle espressioni regolari in Rust: https://www.youtube.com/watch?v=F7Goso5EFk4
- Altri articoli sulle espressioni regolari in Rust: https://www.rustsim.org/blog/regex-part-1-introduction/