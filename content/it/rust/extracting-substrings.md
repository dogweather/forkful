---
title:                "Rust: Estrazione di sottostringhe"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Estrarre sottostringhe è una pratica comune nella programmazione per una varietà di scopi, come la manipolazione di stringhe, la ricerca di pattern e l'elaborazione dei dati. In questo articolo, esploreremo come farlo utilizzando il linguaggio di programmazione Rust.

## Come fare

Per iniziare, dobbiamo importare la libreria standard di Rust per la manipolazione delle stringhe, `str`, utilizzando la seguente dichiarazione:

```Rust
use std::str;
```

Successivamente, possiamo utilizzare il metodo `slice` per estrarre una sottostringa dalla stringa originale. Prendiamo ad esempio la seguente stringa:

```Rust
let stringa = "Questo è un esempio di stringa";
```

Per estrarre una sottostringa che inizia dalla quinta posizione e termina alla dodicesima posizione, possiamo utilizzare il seguente codice:

```Rust
let sottostringa = &stringa[5..12];
```

In questo caso, `sottosubstr` conterrà la sottostringa "è un ese". Possiamo anche estrarre una sottostringa che inizia da una determinata posizione e continua fino alla fine della stringa utilizzando il seguente codice:

```Rust
let sottostringa = &stringa[11..];
```

In questo caso, `sottostringa` conterrà la sottostringa "esempio di stringa".

È anche possibile utilizzare il metodo `chars` per estrarre una singola lettera dalla stringa originale. Ad esempio, per estrarre la terza lettera della stringa, possiamo utilizzare il seguente codice:

```Rust
let terza_lettera = &stringa.chars().nth(2).unwrap();
```

In questo caso, `terza_lettera` conterrà la lettera "u".

## Approfondimento

Per una comprensione più approfondita dell'estrazione delle sottostringhe in Rust, è utile conoscere i concetti di ownership e borrowing del linguaggio. Quando estraiamo una sottostringa da una stringa, non stiamo creando una nuova stringa, ma semplicemente facendo riferimento alla porzione di memoria che contiene la sottostringa. Ciò significa che la sottostringa condividerà lo stesso spazio di memoria della stringa originale e qualsiasi modifica alla sottostringa influenzerà anche la stringa originale.

Inoltre, è importante notare che quando si utilizza il metodo `slice`, il primo indice è incluso nella sottostringa, ma il secondo indice non lo è. Ad esempio, nell'esempio precedente, la sottostringa inizia alla quinta posizione e si ferma alla dodicesima, ma il carattere alla dodicesima posizione non è incluso nella sottostringa. Questo è importante da considerare quando si sceglie l'intervallo di posizioni per l'estrazione della sottostringa.

## Vedi anche

Per ulteriori informazioni sull'utilizzo dei metodi di manipolazione delle stringhe in Rust, consultare la documentazione ufficiale di Rust sulle stringhe: https://doc.rust-lang.org/std/string/.

Per esempi pratici su come estrarre sottostringhe in altri linguaggi di programmazione, leggere questo articolo su GeeksforGeeks: https://www.geeksforgeeks.org/substring-in-a-string/.

Infine, per ulteriori risorse su Rust, si consiglia di visitare il sito ufficiale del linguaggio: https://www.rust-lang.org/it-BR/.