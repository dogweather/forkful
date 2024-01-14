---
title:    "Rust: Estrazione di sottostringhe"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore che usa Rust, probabilmente sei consapevole delle numerose funzionalità e prestazioni di questo linguaggio. Ma forse non sei ancora sicuro se sia il giusto per te. Una delle funzionalità che potresti trovare utile in alcune situazioni è l'estrazione di sottostringhe, che ti consente di selezionare una porzione specifica di una stringa. Ma perché dovresti voler farlo? Continua a leggere per scoprire perché e come farlo.

## Come fare

Per estrarre una sottostringa in Rust, puoi utilizzare il metodo `get()` sulla stringa di cui si desidera ottenere una porzione. Ad esempio, se vuoi estrarre una sottostringa di tre caratteri a partire dalla posizione 2 di una stringa, puoi utilizzare il seguente codice:

```Rust
let stringa = "ciao";
let sottostringa = stringa.get(2..5);
```

In questo esempio, la sottostringa sarà "iao". Si noti che il metodo `get()` utilizza un range per selezionare la porzione desiderata della stringa.

Ma cosa succede se vuoi estrarre una sottostringa di una lunghezza specifica a partire da una posizione specifica? In questo caso, puoi utilizzare il metodo `slice()` al posto di `get()`. Ad esempio:

```Rust
let stringa = "ciao";
let sottostringa = stringa.slice(1, 3);
```

In questo caso, la sottostringa sarà "ia".

## Approfondimento

Se desideri un approfondimento sulla logica alla base dell'estrazione di sottostringhe in Rust, puoi consultare la documentazione ufficiale su `str::get()` e `str::slice()`. Inoltre, esplorare l'implementazione di questi metodi nei file di codice sorgente di Rust può darti una maggiore comprensione del funzionamento dei metodi e di come puoi adattarli alle tue esigenze specifiche.

## Vedi anche

- Documentazione ufficiale su `str::get()`: https://doc.rust-lang.org/std/primitive.str.html#method.get
- Documentazione ufficiale su `str::slice()`:https://doc.rust-lang.org/std/primitive.str.html#method.slice