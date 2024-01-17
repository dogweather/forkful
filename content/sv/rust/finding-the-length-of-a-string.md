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

## Vad & Varför?
Hitta längden av en sträng är en vanlig uppgift för programmerare. Det handlar helt enkelt om att räkna antalet tecken i en given sträng, oavsett om det är bokstäver, siffror eller specialtecken. Detta görs ofta för att kunna behandla strängar på ett effektivt sätt i en programkod.

## Så här gör du:
```Rust
let my_string = "Hej från Sverige!";

// Använd funktionen .len() för att hitta längden av strängen
let length = my_string.len();

// Skriv ut resultatet
println!("Längden av strängen är: {}", length);
```

Output:
`Längden av strängen är: 18`

## Deep Dive
Intressant nog är konceptet att hitta längden av en sträng inte unikt för programmeringsspråket Rust. Det finns andra språk som också har inbyggda funktioner för detta, som till exempel Python (len()) och Java (length()). I historisk kontext är det här en viktig funktion eftersom det hjälper till att hantera och behandla textdata på ett effektivt sätt.

Ett annat sätt att hitta längden av en sträng är genom att loopa igenom varje tecken och räkna antalet. Detta kan dock vara mindre effektivt och mer tidskrävande, särskilt när det gäller längre strängar.

I Rust är det värt att notera att längden av en sträng faktiskt mäts i byte, inte tecken. Detta kan vara viktigt att komma ihåg om du hanterar flerspråkiga program där tecken kan ha olika byte-längd.

## Se även
Läs mer om strängar och hur de hanteras i Rust på [Rust-std.org](https://doc.rust-lang.org/std/string/index.html).