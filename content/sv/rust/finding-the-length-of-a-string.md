---
title:                "Rust: Avgöra längden på en sträng"
simple_title:         "Avgöra längden på en sträng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att kunna hitta längden på en sträng är en viktig del av programmering, oavsett vilket språk du använder. Det låter dig hantera textdata på ett effektivt sätt och utföra olika operationer.

## Hur man gör
För att hitta längden på en sträng i Rust, använder man sig av `len()` funktionen som finns tillgänglig inbyggt i språket. För att få reda på längden av en specifik sträng behöver du bara skriva `sträng.len()` i din kod, där "sträng" är namnet på den specifika strängen du vill undersöka. Kom ihåg att alltid deklarera och initiera strängen innan du använder `len()` funktionen på den.

```Rust
fn main() {
    let str = "Hej från Sverige!";
    println!("Längden på strängen är: {}", str.len());
}
```

Output:
```
Längden på strängen är: 17
```

## Djupdykning
I Rust är strängar en del av "Primitive Data Types" och är representerade som en serie av bytes. Det betyder att när du använder `len()` funktionen för att hitta längden på en sträng, så returnerar den faktiskt antalet bytes som strängen innehåller. Detta är viktigt att ha i åtanke eftersom olika tecken och symboler tar upp olika antal bytes i en sträng. Till exempel tar ett latinskt tecken upp ett byte, medan ett kinesiskt tecken tar upp tre bytes.

## Se även
- [Officiell Rust dokumentation om strängar](https://doc.rust-lang.org/std/string/struct.String.html)
- [Enkelt exempel på att hitta längden på en sträng i Rust](https://www.tutorialspoint.com/rust/rust_strings.htm)
- [Rust programmeringsguide för nybörjare](https://www.geeksforgeeks.org/rust-programming-language-introduction/)