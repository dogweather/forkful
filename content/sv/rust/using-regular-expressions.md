---
title:    "Rust: Användning av reguljära uttryck"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Reguljära uttryck (regular expressions) är en kraftfull funktion inom programmering som tillåter programmerare att enkelt söka och matcha mönster i textsträngar. Genom att använda reguljära uttryck, kan du hitta och extrahera specifika delar av texten du arbetar med, vilket kan spara tid och göra din kod mer effektiv.

## Hur man gör det

För att använda reguljära uttryck i Rust, måste du först importera biblioteket "Regex". Du kan göra detta genom att lägga till `use regex::Regex;` i din kodbas. Sedan kan du skapa ett nytt Regex-objekt genom att använda `Regex::new(pattern)`, där mönstret är det reguljära uttrycket som du vill matcha.

```Rust
use regex::Regex;

let pattern = Regex::new(r"hello");
```

För att söka efter en matchning i en textsträng, använder du `is_match`-metoden.

```Rust
let text = "Hello World!";
if pattern.is_match(text) {
    println!("Match found!");
} else {
    println!("No match found.");
}
```

För att extrahera en del av texten baserat på en matchning, kan du använda `captures`-metoden.

```Rust
let text = "Hello World!";
match pattern.captures(text) {
    Some(captures) => {
        println!("Match found: {}", captures.get(0).unwrap().as_str());
    },
    None => println!("No match found.");
}
```

## Mer detaljerad information

Det reguljära uttrycket `r"hello"` som används i vårt exempel är enkelt och söker bara efter en exakt matchning av strängen "hello". Men reguljära uttryck kan vara mycket mer kraftfulla än så. De kan innehålla specialtecken och -teckenklasser som hjälper dig att hitta mönster relaterade till stora delar av texten, såsom siffror, bokstäver och skiljetecken.

Du kan också använda så kallade fångstgrupper (capture groups) för att extrahera specifika delar av texten och använda dessa i din kod. Till exempel kan du använda `r"(\d{2})/(\d{2})/(\d{4})"` för att hitta och extrahera datum i formatet "DD/MM/YYYY".

Reguljära uttryck är också användbara för validering av inmatade data. Genom att matcha inmatad text mot ett specifikt reguljärt uttryck kan du enkelt kontrollera att det har rätt format eller struktur.

## Se även

- [Rust Regex biblioteket](https://docs.rs/regex/1.5.4/regex/)
- [Reguljära uttryck tutorial på svenska](https://www.geeksforgeeks.org/regex-progaramming/)
- [Reguljära uttryck guide för nybörjare](https://www.freecodecamp.org/news/a-beginners-guide-to-regular-expressions-regex-in-javascript/)