---
title:    "Rust: Unione di stringhe"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Ci sono diverse ragioni per cui potresti voler concatenare stringhe nel tuo codice Rust. Ad esempio, potresti voler creare un'unica stringa composta da vari elementi, o forse hai bisogno di manipolare una stringa esistente per ottenere un risultato desiderato. Indipendentemente dalla tua specifica esigenza, concatenare stringhe è un'operazione comune e importante nella programmazione.

## Come Fare

La concatenazione di stringhe in Rust è abbastanza semplice. Basta utilizzare l'operatore `+` per unire due stringhe. Ad esempio:

```Rust
let prima_stringa = "Ciao";
let seconda_stringa = " mondo!";
let risultato = prima_stringa + seconda_stringa;
```

L'output di questo codice sarà la stringa "Ciao mondo!". È importante notare che il risultato è una nuova stringa, poiché le stringhe in Rust sono immutabili per impostazione predefinita. Ciò significa che ogni volta che concateniamo due stringhe, stiamo creando una nuova stringa invece di modificarne una esistente.

Inoltre, se desideri concatenare più di due stringhe, puoi utilizzare il metodo `format!` che accetta un numero qualsiasi di argomenti e li unisce in una singola stringa. Ad esempio:

```Rust
let nome = "Mario";
let cognome = "Rossi";
let eta = 30;
let output = format!("Il mio nome è {}, il mio cognome è {} e ho {} anni.", nome, cognome, eta);
```

L'output di questo codice sarà "Il mio nome è Mario, il mio cognome è Rossi e ho 30 anni."

## Approfondimento

Sebbene la concatenazione di stringhe sembri un'operazione semplice, ci sono alcuni aspetti da tenere in considerazione quando si lavora con stringhe in Rust. Ad esempio, come accennato in precedenza, le stringhe sono immutabili per impostazione predefinita. Ciò significa che ogni volta che concateniamo due stringhe, stiamo creando una nuova stringa invece di modificare quella esistente. Ciò può causare problemi di prestazioni se stiamo manipolando molti dati o siamo in un ambiente con risorse limitate.

Inoltre, è importante tenere presente che le stringhe in Rust sono codificate in UTF-8, il che significa che alcune operazioni di concatenazione potrebbero non funzionare come ci si aspetta se le stringhe contengono caratteri non-ASCII. Per risolvere questo problema, è possibile utilizzare la tipizzazione corretta e gestire correttamente gli errori di codifica.

## Vedi Anche

- [Documentazione ufficiale sulle stringhe in Rust](https://doc.rust-lang.org/std/string/index.html)
- [Come gestire correttamente le stringhe UTF-8 in Rust](https://unicode-rs.github.io/unicode-rs)
- [Esempi di concatenazione di stringhe in Rust](https://github.com/rust-lang/rust-by-example/blob/master/std/ops/strings.md)