---
date: 2024-01-20 17:43:05.918600-07:00
description: "Hur man g\xF6r: I exemplet ovan anv\xE4nds `regex`-paketet f\xF6r att\
  \ matcha och ta bort tecken baserat p\xE5 ett specifikt m\xF6nster. Efter att koden\
  \ k\xF6rs f\xE5r vi\u2026"
lastmod: '2024-04-05T21:53:39.011055-06:00'
model: gpt-4-1106-preview
summary: "I exemplet ovan anv\xE4nds `regex`-paketet f\xF6r att matcha och ta bort\
  \ tecken baserat p\xE5 ett specifikt m\xF6nster."
title: "Ta bort tecken som matchar ett m\xF6nster"
weight: 5
---

## Hur man gör:
```Rust
fn main() {
    let text = "Hej, hur mår du? 123";
    let pattern = "[^a-zA-Z åäöÅÄÖ]"; // Mönstret matchar allt som inte är bokstäver eller mellanslag
    let cleaned_text = regex::Regex::new(pattern).unwrap().replace_all(&text, "");
    println!("Rensad text: {}", cleaned_text);
}
```
I exemplet ovan används `regex`-paketet för att matcha och ta bort tecken baserat på ett specifikt mönster. Efter att koden körs får vi utskriften:

```
Rensad text: Hej, hur mår du
```

## Fördjupning
Borttagning av tecken baserat på mönster är inget nytt, och har sitt ursprung i tidiga text-processeringsverktyg som `sed` och `awk`. Rost erbjuder flera alternativ för detta; `regex`-paketet är dock det mest robusta och flexibla verktyget för att hantera komplexa mönster. Implementationen använder "lazy DFA" vilket innebär snabb och effektiv matchning. Alternativ till `regex` inkluderar att använda `str`-metoder som `replace()` för enklare byten, men dessa saknar möjligheten att använda mönster.

## Se även
- Rust `regex` dokumentation: https://docs.rs/regex/
- Rust sträng-dokumentation: https://doc.rust-lang.org/std/string/struct.String.html
- `sed` och `awk`: två text-processeringsverktyg i UNIX som kan inspirera när man hanterar textmönster.
