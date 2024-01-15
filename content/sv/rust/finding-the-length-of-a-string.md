---
title:                "Att hitta längden på en sträng"
html_title:           "Rust: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att veta längden på en sträng är en grundläggande egenskap som kan hjälpa dig att manipulera data och skapa effektivare program. Det är också en viktig färdighet att ha när du lär dig att programmera i Rust.

## Så här gör du

För att hitta längden på en sträng i Rust, kan du använda inbyggda funktionen `len()` som finns i standardbiblioteket `std::str`.

```Rust
let str = "Hej Rust";
let length = str.len();
println!("Längden på strängen är: {}", längd);
```

Output:

```
Längden på strängen är: 8
```

För att använda `len()` funktionen behöver du först skapa en variabel som innehåller strängen du vill mäta längden på. Sedan kallar du på `len()` funktionen och tilldelar resultatet till en annan variabel, i detta fall `length`.

Om du vill kan du också använda `len()` funktionen direkt på en sträng utan att tilldela den till en variabel.

```Rust
let str = "Hej Rust";
println!("Längden på strängen är: {}", str.len());
```

Output:

```
Längden på strängen är: 8
```

En annan metod att hitta längden på en sträng är att använda `str.len_utf8()`. Detta är särskilt användbart om du arbetar med strängar som innehåller Unicode-tecken.

## Deep Dive

I Rust lagras strängar som en sekvens av bytes. `len()` funktionen räknar bara antalet bytes i en sträng och detta kan vara fel om du använder Unicode. Det är här `str.len_utf8()` kommer in i bilden. Den här funktionen räknar istället antalet Unicode-tecken i en sträng. Detta är viktigt eftersom vissa Unicode-tecken tar upp mer än en byte.

Tänk på den här strängen: "𠜎". Den består av två Unicode-tecken men tar upp 4 bytes i en vanlig sträng. Om vi använder `len()` funktionen får vi ett felaktigt svar.

```Rust
let str = "𠜎";
println!("Längden på strängen är: {}", str.len());
```

Output:

```
Längden på strängen är: 4
```

Men om vi använder `str.len_utf8()` får vi det korrekta svaret.

```Rust
let str = "𠜎";
println!("Längden på strängen är: {}", str.len_utf8());
```

Output:

```
Längden på strängen är: 2
```

Det är viktigt att välja rätt funktion beroende på om du behöver räkna antalet bytes eller Unicode-tecken i en sträng.

## Se även

För mer information om strängmanipulering i Rust, se nedan:

- [Rust Standardbibliotek: str - Utforska inbyggda strängfunktioner](https://doc.rust-lang.org/std/str/index.html)
- [Officiell Rust Dokumentation - Str strings](https://doc.rust-lang.org/book/ch08-02-strings.html)