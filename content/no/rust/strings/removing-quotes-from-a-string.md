---
date: 2024-01-26 03:41:46.337564-07:00
description: "\xC5 fjerne anf\xF8rselstegn fra en streng i Rust handler om \xE5 stripe\
  \ bort un\xF8dvendige ekstra anf\xF8rselstegn som kan v\xE6re pakket rundt tekstdataene\
  \ dine.\u2026"
lastmod: '2024-03-13T22:44:40.562108-06:00'
model: gpt-4-0125-preview
summary: "\xC5 fjerne anf\xF8rselstegn fra en streng i Rust handler om \xE5 stripe\
  \ bort un\xF8dvendige ekstra anf\xF8rselstegn som kan v\xE6re pakket rundt tekstdataene\
  \ dine."
title: "Fjerne anf\xF8rselstegn fra en streng"
weight: 9
---

## Hvordan:
```Rust
fn remove_quotes(s: &str) -> String {
    s.trim_matches(|c| c == '\"' || c == '\'').to_string()
}

fn main() {
    let quoted_str = "\"Hei, Rustaceans!\"";
    let cleaned_str = remove_quotes(quoted_str);
    println!("{}", cleaned_str);
    // Utdata: Hei, Rustaceans!
}
```

Noen ganger har du en streng med blandete anførselstegn, slik som dette:

```Rust
fn main() {
    let mixed_quoted = "'Rust sier: \"Hallo, Verden!\"'";
    let cleaned_str = remove_quotes(mixed_quoted);
    println!("{}", cleaned_str);
    // Utdata: Rust sier: "Hallo, Verden!"
}
```

Her fjernes bare de ytterste enkle anførselstegnene.

## Dypdykk
Når du fjerner anførselstegn fra en streng, lurer du kanskje på hvorfor det ikke er bare en enkel `.replace("\"", "")`. Tidligere var tekstbehandling mindre standardisert, og ulike systemer hadde forskjellige måter å lagre og overføre tekst på, ofte med en slags 'escape-sekvens' for spesielle tegn. Rusts `trim_matches`-metode er mer allsidig, og lar deg spesifisere flere tegn å trimme, og om du skal trimme fra starten (prefiks), slutten (suffiks), eller begge sider av strengen.

Det finnes alternativer, selvfølgelig. Regex er en kraftpakke for strengmanipulering, i stand til å matche komplekse mønstre, og ville være for mye for bare å fjerne anførselstegn. Biblioteker som `trim_in_place` kunne tilby trimming på stedet uten overheaden av å skape et nytt `String`-objekt, noe som kan være ønskelig for applikasjoner der ytelse er kritisk.

Under panseret itererer `trim_matches` faktisk gjennom strengens tegn fra begge ender og sjekker mot det angitte mønsteret til et ikke-samsvarerende tegn er funnet. Det er effektivt for det den gjør, men vær alltid klar over at den jobber med Unicode-skalare verdier. Hvis strengen din kan inneholde flerbyte Unicode-tegn, trenger du ikke å bekymre deg for at den bryter dem opp.

## Se Også
- Rusts dokumentasjon om strengmanipulering: https://doc.rust-lang.org/book/ch08-02-strings.html
- `regex`-pakken for komplekse mønstre: https://crates.io/crates/regex
- Rust ved Eksempel for praktiske kodscenarioer: https://doc.rust-lang.org/stable/rust-by-example/std/str.html
