---
title:    "Rust: ESTRATTI DI SOTTOSTRINGHE"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per cui potresti voler estrarre delle sottostringhe da una stringa più grande. Ad esempio, potresti aver bisogno di manipolare una stringa ottenuta da un input dell'utente, o potresti aver bisogno di dividere una stringa in più parti per elaborarla in modo più efficiente.

## Come fare
Per estrarre una sottostringa da una stringa, possiamo utilizzare il metodo `get` o `get_mut` dell'`&str` o utilizzare il metodo `split` per dividere la stringa in più parti.

```Rust
let my_string = "Ciao ragazzi!";
// Estrarre la sottostringa "Ciao"
let saluto = &my_string[0..4];
// Dividere la stringa in più parti usando lo spazio come separatore
for parola in my_string.split(" ") {
    println!("{}", parola);
}
```

Output:
```
Ciao
ragazzi!
```

## Approfondimento
Quando estraiamo una sottostringa, è importante tenere conto dei byte della stringa originale e della codifica utilizzata. Inoltre, è possibile utilizzare il metodo `chars` per iterare sui caratteri della stringa anziché sui byte. Ricorda anche che le sottostringhe in Rust non vengono allocate di nuovo, ma semplicemente fanno riferimento alla sezione di memoria della stringa originale.

## Vedi anche
- Documentazione ufficiale di Rust per i metodi di estrazione di sottostringhe ([Doc](https://doc.rust-lang.org/std/primitive.str.html#method.get))
- Un esempio pratico di utilizzo delle sottostringhe in Rust ([Articolo](https://www.tutorialspoint.com/rust/rust_substrings.htm))
- Un video tutorial su come estrarre le sottostringhe in Rust ([Video](https://www.youtube.com/watch?v=GYLcRx2cL8M))