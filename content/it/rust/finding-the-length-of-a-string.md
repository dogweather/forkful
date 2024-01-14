---
title:    "Rust: Trovare la lunghezza di una stringa."
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Molti programmatori sono incuriositi dalla sintassi pulita e dalla sicurezza di Rust, e desiderano imparare a gestire le stringhe in modo efficiente. Conoscere il modo corretto di trovare la lunghezza di una stringa è un passo importante nel processo di apprendimento della programmazione in Rust.

## Come fare

Per trovare la lunghezza di una stringa in Rust, possiamo utilizzare il metodo `len()` incorporato. Questo metodo restituisce il numero di byte presenti nella stringa data. Vediamo un esempio pratico:

```Rust
fn main() {
  let s = "Ciao, mondo!";

  println!("La lunghezza della stringa è {}", s.len());
}
```

Output:

```
La lunghezza della stringa è 13
```

Per trovare la lunghezza in caratteri e non in byte, possiamo utilizzare il metodo `chars().count()`. Questo metodo restituisce il numero di caratteri presenti nella stringa data. Vediamo un altro esempio pratico:

```Rust
fn main() {
  let s = "Ciao, mondo!";

  println!("La lunghezza della stringa in caratteri è {}", s.chars().count());
}
```

Output:

```
La lunghezza della stringa in caratteri è 12
```

## Approfondimento

Potresti chiederti come mai la lunghezza della stringa in caratteri sia 12, mentre prima abbiamo ottenuto 13 come lunghezza in byte. La risposta sta nel fatto che il carattere "à" nella frase "Ciao, mondo!" occupa due byte, ma viene contato come un unico carattere.

Inoltre, è importante notare che il metodo `len()` conta solo i byte nella stringa, senza considerare se sono rappresentati da un singolo carattere o da più caratteri. Per esempio, il carattere "👋" (mano che saluta) occupa 4 byte, ma viene contato come un unico carattere nella lunghezza della stringa.

In sostanza, il modo corretto di ottenere la lunghezza di una stringa dipende dal contesto e dal tipo di dati che stiamo utilizzando. È importante comprendere le differenze tra la lunghezza in byte e la lunghezza in caratteri per evitare errori nei nostri programmi.

## Vedi anche

- [Documentazione ufficiale di Rust su stringhe](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Tutorial di programmazione in Rust](https://www.rust-lang.org/learning)
- [Altri approfondimenti sulla gestione delle stringhe in Rust](https://www.regular-expressions.info/rust.html)