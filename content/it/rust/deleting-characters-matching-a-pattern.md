---
title:    "Rust: Eliminare i caratteri corrispondenti a un modello"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Perché

La cancellazione di caratteri che corrispondono a un certo pattern può essere un'operazione molto utile in alcune situazioni, come ad esempio nel processo di pulizia dei dati o nella gestione delle stringhe. In questo articolo vi spiegherò come farlo utilizzando il linguaggio di programmazione Rust.

## Come Fare

Per prima cosa, dobbiamo importare la libreria Regex di Rust per poter utilizzare le espressioni regolari. Possiamo farlo nel nostro codice con la seguente linea:

```Rust
use regex::Regex;
```

Successivamente, dobbiamo creare un oggetto Regex che rappresenti il pattern da cercare. Possiamo farlo utilizzando il metodo `new` e passando come parametro una stringa contenente l'espressione regolare. Ad esempio, se vogliamo cancellare tutte le vocali da una stringa, possiamo usare il seguente codice:

```Rust
let re = Regex::new("[aeiou]").unwrap();
```

Ora che abbiamo il nostro oggetto Regex, possiamo utilizzarlo per sostituire i caratteri corrispondenti al pattern con una stringa vuota, utilizzando il metodo `replace_all` e passando come secondo parametro una stringa vuota. Ad esempio, possiamo sostituire le vocali nella stringa "ciao mondo" con il seguente codice:

```Rust
let result = re.replace_all("ciao mondo", "");
```

Il valore della variabile `result` sarà ora "c mnd". Possiamo stampare il risultato a schermo utilizzando il metodo `println`:

```Rust
println!("{}", result);
```

E otterremo il seguente output:

```
c mnd
```

## Deep Dive

L'utilizzo delle espressioni regolari ci permette di specificare pattern più complessi e di utilizzare dei "wildcard" per effettuare sostituzioni più avanzate. Ad esempio, possiamo utilizzare il simbolo `.` per indicare qualsiasi carattere e il simbolo `+` per indicare una o più ripetizioni del carattere precedente. Quindi, se volessimo cancellare tutti i numeri presenti in una stringa, possiamo utilizzare il seguente pattern:

```
[0-9]+
```

Utilizzando l'esempio di prima, se aggiungiamo il pattern "[0-9]" alla nostra espressione regolare, il risultato finale sarà "c mndo".

## Vedi Anche

- [Documentazione della libreria Regex di Rust](https://docs.rs/regex/)
- [Espressioni regolari in Rust](https://danielkeep.github.io/tlborm/book/recipes/regex.html)
- [Tutorial sulle espressioni regolari in Rust](https://riptutorial.com/rust/example/4409/basic-regular-expressions)