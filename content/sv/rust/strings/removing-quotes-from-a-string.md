---
date: 2024-01-26 03:42:17.031046-07:00
description: "Hur man g\xF6r: Ibland kan du ha en str\xE4ng med blandade citattecken,\
  \ som denna."
lastmod: '2024-04-05T21:53:39.014663-06:00'
model: gpt-4-0125-preview
summary: "Ibland kan du ha en str\xE4ng med blandade citattecken, som denna."
title: "Ta bort citattecken fr\xE5n en str\xE4ng"
weight: 9
---

## Hur man gör:
```Rust
fn remove_quotes(s: &str) -> String {
    s.trim_matches(|c| c == '\"' || c == '\'').to_string()
}

fn main() {
    let quoted_str = "\"Hej, Rustaceans!\"";
    let cleaned_str = remove_quotes(quoted_str);
    println!("{}", cleaned_str);
    // Output: Hej, Rustaceans!
}
```

Ibland kan du ha en sträng med blandade citattecken, som denna:

```Rust
fn main() {
    let mixed_quoted = "'Rust säger: \"Hej, Världen!\"'";
    let cleaned_str = remove_quotes(mixed_quoted);
    println!("{}", cleaned_str);
    // Output: Rust säger: "Hej, Världen!"
}
```

Här tas endast de yttersta enkla citattecknen bort.

## Djupdykning
När du tar bort citattecken från en sträng kan du undra varför det inte bara är en enkel `.replace("\"", "")`. Tidigt om, var hanteringen av text mindre standardiserad, och olika system hade olika sätt att lagra och överföra text på, ofta med någon form av 'escape-sekvens' för speciella tecken. Rusts `trim_matches`-metod är mer mångsidig, vilket låter dig specificera flera tecken att trimma, och om du ska trimma från början (prefix), slutet (suffix) eller båda sidorna av strängen.

Det finns alternativ, naturligtvis. Regex är kraftpaketet för strängmanipulation, kapabelt att matcha komplexa mönster, och skulle vara overkill för bara att ta bort citattecken. Bibliotek som `trim_in_place` kan erbjuda trimning på plats utan overheaden av att skapa ett nytt `String`-objekt, vilket kan vara önskvärt för prestandakritiska applikationer.

Innanför skalet itererar `trim_matches` faktiskt igenom strängens tecken från båda ändarna, kontrollerar mot det angivna mönstret tills ett icke-matchande tecken hittas. Det är effektivt för vad det gör, men var alltid medveten om att det arbetar med Unicode skalärvärden. Om din sträng kan innehålla flerbytes Unicode-tecken, behöver du inte oroa dig för att den bryter upp dem.

## Se även
- Rusts dokumentation om strängmanipulation: https://doc.rust-lang.org/book/ch08-02-strings.html
- `regex`-lådan för komplexa mönster: https://crates.io/crates/regex
- Rust genom Exempel för praktiska kodningsscenarier: https://doc.rust-lang.org/stable/rust-by-example/std/str.html
