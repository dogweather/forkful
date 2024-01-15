---
title:                "Sökning och ersättning av text"
html_title:           "Rust: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Varför
En av de vanligaste uppgifterna inom programmering är att söka och ersätta text i en fil eller en sträng. Det kan vara för att korrigera stavfel, byta ut namn eller helt enkelt uppdatera innehållet. Med Rusts inbyggda funktioner för sökning och ersättning blir denna process både enkel och effektiv.

# Såhär gör du
För att använda Rusts inbyggda funktioner för sökning och ersättning behöver du först importera standardbiblioteket "std::str". Sedan kan du använda funktionerna "replace" och "replace_first" för att söka efter en specifik text och ersätta den med en given sträng. 

```
use std::str;

let original = "Hej världen!";
let ny_text = str::replace(original, "Hej", "Hallå"); // ersätter "Hej" med "Hallå"
let ny_text2 = str::replace_first(original, "Hej", "Hallå"); // ersätter endast den första förekomsten av "Hej" med "Hallå"

println!("Originalsträng: {}", original);
println!("Ny sträng: {}", ny_text);
println!("Ny sträng 2:  {}", ny_text2);
```

Output:
```
Originalsträng: Hej världen!
Ny sträng: Hallå världen!
Ny sträng 2:  Hallå världen!
```

# Djupdykning
Utöver de inbyggda funktionerna för sökning och ersättning finns det även en rad olika bibliotek och paket som kan hjälpa till med denna uppgift. Ett populärt paket är "regex" som ger möjlighet att använda reguljära uttryck för att söka och ersätta text. Detta kan vara speciellt användbart vid mer avancerade sökningar där man vill hitta mönster i texten istället för specifika ord eller fraser.

# Se även
- [Rust Standardbiblioteket](https://doc.rust-lang.org/stable/std/)
- [Regex paketet för Rust](https://crates.io/crates/regex)