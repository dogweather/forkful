---
title:                "Sammanfogning av strängar"
html_title:           "Rust: Sammanfogning av strängar"
simple_title:         "Sammanfogning av strängar"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Om du någonsin har hanterat data som innehåller textsträngar, så har du antagligen stött på behovet av att sammanslå flera strängar till en enda. Rust har en enkel och effektiv metod för att göra just det. 

## Så här gör man

För att sammanslå två strängar i Rust, kan du använda "+" operatorn.

```rust
let fornamn = "Lisa";
let efternamn = "Andersson";

let hela_namnet = fornamn + efternamn;
println!("{}", hela_namnet);
```
Output:
```
LisaAndersson
```

Du kan också sammanslå flera strängar på en gång genom att använda "format!" macro:n. Denna metod använder "placeholder" för att hålla reda på vilken ordning strängarna ska sättas ihop i.

```rust
let favorit_mat = "Pizza";
let favorit_dryck = "Coca Cola";

let beskrivning = format!("Jag älskar {} med en kall {} till!", favorit_mat, favorit_dryck);
println!("{}", beskrivning);
```
Output:
```
Jag älskar Pizza med en kall Coca Cola till!
```

## Deep Dive

När du använder "+" operatorn för att sammanslå strängar, så kan dessa inte ändras i efterhand. Detta beror på att strängar i Rust är immutabla, vilket innebär att de inte kan ändras efter att de har skapats. Om du försöker ändra en sammanslagen sträng, så kommer du få ett felmeddelande.

För att lösa detta, kan du använda en "String" typ istället för en "str" typ. En "String" är en dynamiskt allokerad sträng som kan ändras efter att den har skapats. För att sammanslå strängar med typen "String", kan du använda "format!"macro:n eller "push_str" funktionen.

```rust
let mut fornamn = String::from("Lisa");
let efternamn = "Andersson";

fornamn.push_str(efternamn);
println!("{}", fornamn);
```
Output:
```
LisaAndersson
```

## Se också

- [Rust String Dokumentation](https://doc.rust-lang.org/std/string/struct.String.html)
- [Tutorial: Strings in Rust](https://www.youtube.com/watch?v=xD5slrrhkpc)
- [Översikt av Rust's Data typer](https://doc.rust-lang.org/book/ch03-02-data-types.html)