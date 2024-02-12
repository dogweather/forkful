---
title:                "Ta bort tecken som matchar ett mönster"
aliases:
- /sv/rust/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:43:05.918600-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ta bort tecken som matchar ett mönster"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ta bort tecken utifrån ett mönster handlar om att rensa strängar från oönskade sekvenser. Programmerare gör detta för att sanera data, extrahera information, eller förbereda text för vidare processer.

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
