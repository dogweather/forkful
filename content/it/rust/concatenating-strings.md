---
title:                "Concatenazione di stringhe"
html_title:           "Rust: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché
Concatenare le stringhe è una pratica comune nella programmazione per combinare più stringhe in una sola. Questo può essere utile per creare messaggi personalizzati, formattare testo e molte altre applicazioni.

## Come Fare
In Rust, è possibile concatenare le stringhe utilizzando l'operatore `+` o il metodo `format!()`. Ecco un esempio di entrambi i metodi utilizzando le stringhe "Ciao" e "mondo":

```Rust
let saluto = "Ciao";
let messaggio = saluto + " mondo!"; // utilizzando l'operatore +
println!("{}", messaggio);
// Output: Ciao mondo!

let messaggio = format!("{} mondo!", saluto); // utilizzando il metodo format!()
println!("{}", messaggio);
// Output: Ciao mondo!
```

Come si può vedere dall'esempio, l'operatore `+` concatena due stringhe in una nuova stringa, mentre il metodo `format!()` permette di combinare più stringhe all'interno di una sola stringa utilizzando il placeholder `{}`.

## Approfondimento
In Rust, le stringhe sono immutabili, quindi quando si concatenano due stringhe, in realtà si crea una nuova stringa e si scarta quella vecchia. Ad esempio, se si utilizza l'operatore `+` in un ciclo, si creeranno continuamente nuove stringhe, il che può essere inefficiente. 

Per ovviare a questo problema, Rust offre il tipo `String` che è mutabile e permette di aggiungere nuove stringhe senza doverne creare una nuova ogni volta. Si può convertire facilmente una stringa in un tipo `String` utilizzando il metodo `to_string()`. Ecco un esempio:

```Rust
let mut nome = "Maria".to_string(); // convertire una stringa in un tipo String
nome.push_str(" Rossi"); // aggiungere una nuova stringa senza creare una nuova stringa
println!("{}", nome);
// Output: Maria Rossi
```

Inoltre, è possibile utilizzare il tipo `String` in combinazione con il metodo `push()` per aggiungere singoli caratteri alla fine della stringa. Ci sono anche altre operazioni che si possono fare con il tipo `String`, come la rimozione di parte della stringa o la ricerca di una sottostringa. Per ulteriori informazioni, è consigliato consultare la documentazione ufficiale di Rust.

## Vedi Anche
- [La documentazione ufficiale di Rust sulla gestione delle stringhe](https://doc.rust-lang.org/std/string/index.html)
- [Un tutorial su concatenare le stringhe in Rust](https://rust-lang-nursery.github.io/rust-cookbook/text/strings.html#concatenate-strings)
- [Un confronto tra l'operatore `+` e il metodo `format!()` per concatenare le stringhe in Rust](https://stackoverflow.com/questions/30353462/when-does-a-string-get-automatically-converted-into-a-string)