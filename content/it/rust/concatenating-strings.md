---
title:    "Rust: Concatenazione di stringhe"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Perché

Concatenare stringhe è un'operazione comune nella programmazione, che consente di combinare due o più stringhe in una sola stringa più lunga. Questo può essere utile per creare un output di testo più complesso, formattare i dati in una determinata maniera o semplicemente creare un messaggio personalizzato per l'utente.

## Come Fare

Per concatenare stringhe in Rust, è necessario utilizzare l'operatore "+" tra le stringhe da concatenare. Ad esempio:

```Rust
let nome = "Marco";
let cognome = "Rossi";
let nome_completo = nome + " " + cognome;
println!("{}", nome_completo);
```

Output:

`Marco Rossi`

In questo esempio, le variabili "nome" e "cognome" vengono concatenate utilizzando " " (spazio) come separatore per ottenere il nome completo. Si può utilizzare qualsiasi carattere o stringa come separatore, basta aggiungere tra le due variabili.

```Rust
let num1 = "10";
let num2 = "20";
let somma = num1 + "+" + num2;
println!("{}", somma);
```

Output:

`10+20`

## Approfondimento

In Rust, concatenare stringhe può essere un'operazione costosa in termini di performance, soprattutto nel caso di stringhe lunghe. Questo perché ogni volta che una stringa viene concatenata, ne viene creata una nuova copia in memoria.

Per ovviare a questo problema, è possibile utilizzare il tipo di dato "String", che rappresenta una stringa mutabile e permette di modificare la stringa originale senza doverla ricopiare ogni volta. Inoltre, si può utilizzare il metodo "format!" per concatenare più stringhe in una sola volta.

```Rust
let num1 = String::from("10");
let num2 = String::from("20");
let somma = format!("{} + {}", num1, num2);
println!("{}", somma);
```

Output:

`10 + 20`

## Vedi Anche

- [La documentazione ufficiale di Rust su String](https://doc.rust-lang.org/std/string/struct.String.html)
- [Guide e tutorial su Rust](https://www.rust-lang.org/learn)
- [Esempi di codice su concatenazione di stringhe in Rust](https://github.com/rust-lang/rust-by-example/blob/master/std/str.rs)