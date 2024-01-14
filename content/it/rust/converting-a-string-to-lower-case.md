---
title:                "Rust: Trasformare una stringa in minuscolo"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Se sei nuovo alla programmazione in Rust, potresti chiederti perché dovresti preoccuparti di convertire una stringa in caratteri minuscoli. La risposta è semplice: spesso è necessario trattare le stringhe come dati, e convertirle in minuscolo può facilitare la loro elaborazione e confronto.

## Come fare

È possibile convertire una stringa in minuscolo utilizzando il metodo `to_lowercase()` della struttura `String`. Ecco un esempio di codice che mostra come utilizzarlo:

```Rust
let stringa = String::from("Ciao a tutti!");
let minuscolo = stringa.to_lowercase();

println!("{}", minuscolo); //stamperà "ciao a tutti!"
```

Come puoi vedere, il risultato di `to_lowercase()` è una nuova stringa in cui tutti i caratteri sono in minuscolo. Vale anche la pena notare che questo metodo utilizza il formato Unicode per la conversione, quindi funzionerà con caratteri di qualsiasi alfabeto.

## Approfondimento

Se vuoi approfondire la conversione di una stringa in minuscolo, ci sono alcuni aspetti importanti da considerare. Per esempio, la tua stringa potrebbe contenere caratteri speciali o lettere accentate. In questi casi, dovrai fare attenzione a quale metodo usi per la conversione, poiché alcuni potrebbero non gestire correttamente questi casi particolari.

Inoltre, è importante ricordare che la stringa originale non verrà modificata, ma verrà creata una nuova stringa in minuscolo. Questo può essere utile per evitare errori durante l'elaborazione dei dati, ma potrebbe consumare più memoria se hai a che fare con grandi quantità di stringhe.

## Vedi anche

* Documentazione ufficiale di Rust sull'utilizzo di `to_uppercase()`: https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase
* Un tutorial su come manipolare le stringhe in Rust: https://www.sheshbabu.com/posts/rust-string-manipulation/
* Un esempio di utilizzo di `to_lowercase()` per confrontare stringhe in maniera case-insensitive: https://stackoverflow.com/questions/29829453/using-string-contains-in-a-case-insensitive-manner-in-rust