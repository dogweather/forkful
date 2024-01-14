---
title:    "Rust: Att hitta längden av en sträng"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng är en vanlig uppgift inom programmering och kan vara användbar för att kontrollera inmatning, formattera text eller utföra olika manipulationer på strängar.

## Så här gör du

För att hitta längden på en sträng i Rust kan du använda funktionen `len()` från standardbiblioteket. Här är ett exempel på hur det kan se ut:

```Rust
let sträng = String::from("Hej, världen!");
println!("Längden på strängen är: {}", sträng.len());
```

Detta kommer att skriva ut "Längden på strängen är: 13" eftersom det finns 13 tecken i strängen, inklusive mellanslag.

## Djupdykning

När du använder `len()` i Rust så måste du vara medveten om att den faktiskt räknar teckenkoder och inte antal tecken. Detta betyder att en viss teckenkod kan ha mer än ett tecken, vilket påverkar längden på en sträng. Till exempel har det svenska alfabetet fler teckenkoder än det engelska, vilket kan påverka längden på en sträng som innehåller svenska tecken.

En annan viktig aspekt att tänka på är att `len()` returnerar en `usize`-värde, vilket är en osignerad heltalsvariabel som kan vara av olika storlek beroende på ditt system. Detta kan vara viktigt att tänka på när du använder `len()` för att jämföra längden på strängar.

## Se även

- [Rust Documentation: String](https://doc.rust-lang.org/std/string/index.html)
- [Rust Standardbibliotek: UTF-8 coddar](https://doc.rust-lang.org/std/str/fn.u8.len.html)
- [Tutorial: String operations in Rust](https://linuxize.com/post/string-operations-in-rust/)