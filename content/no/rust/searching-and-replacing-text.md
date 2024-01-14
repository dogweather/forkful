---
title:                "Rust: Søking og bytting av tekst"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Hvorfor

Denne bloggposten tar for seg søke- og erstatningsfunksjonaliteten i Rust-programmeringsspråket. Dette er en viktig del av enhver programmeringsoppgave, og det er viktig å ha en god forståelse for hvordan det fungerer. Ved å lære hvordan man kan søke og erstatte tekst i Rust, vil du kunne gjøre endringer i kodebasen din mer effektivt og unngå feil.

# Hvordan gjøre det

For å søke og erstatte tekst i Rust, kan du bruke funksjonene `replace` og `replace_all` tilgjengelig i standardbiblioteket. For å bruke disse funksjonene, må du først importere `std::string::String` og deretter bruke `replace` eller `replace_all` på en streng variabel. 

```Rust
use std::string::String;

fn main() {
    let mut text = String::from("Hei, hvordan har du det?");
    
    // Erstatter den første forekomsten av "hei" med "hallo"
    text = text.replace("hei", "hallo");
    
    // Erstatter alle forekomster av "o" med "a"
    text = text.replace_all("o", "a");
    
    println!("{}", text);
}
```

Denne koden vil gi følgende utskrift:

`Hallo, hvrdan har du dat?`

Når du bruker `replace`-funksjonen, vil den bare erstatte den første forekomsten av den gitte teksten. Hvis du vil erstatte alle forekomster i en streng, må du bruke `replace_all`-funksjonen.

# Dykk dypere

I tillegg til å bruke `replace` og `replace_all` til å søke og erstatte tekst i en streng, kan du også bruke regulære uttrykk for å gjøre mer kompliserte søk og erstatninger. Regulære uttrykk er et kraftig verktøy som kan hjelpe deg med å finne mønstre i tekst og gjøre avanserte endringer i en streng.

For å bruke regulære uttrykk i Rust, må du først importere `regex`-biblioteket. Deretter kan du bruke funksjonen `Regex::new` for å lage et regulært uttrykk, og `replace_all`-funksjonen for å bruke det på en streng.

```Rust
use regex::Regex;

fn main() {
    let text = "Hei, mitt navn er Per. Jeg liker å kode i Rust";
    
    let re = Regex::new(r"i\sRust$").unwrap();
    let new_text = re.replace_all(&text, " i Java").to_string();
    
    println!("{}", new_text);
}
```

Denne koden vil gi følgende utskrift:

`Hei, mitt navn er Per. Jeg liker å kode i Java`

I dette eksempelet brukte vi et regulært uttrykk til å finne og erstatte teksten "i Rust" med "i Java" på slutten av en streng.

# Se også

- Offisiell Rust-dokumentasjon for `replace` og `replace_all` funksjonene: https://doc.rust-lang.org/std/string/struct.String.html#method.replace
- Regex-dokumentasjon for Rust: https://docs.rs/regex/1.3.9/regex/

Takk for at du leste denne bloggposten om å søke og erstatte tekst i Rust. Forhåpentligvis har du nå fått en god forståelse for hvordan du kan bruke denne funksjonaliteten til å gjøre endringer i din egen Rust-kode. Lykke til videre med programmering i Rust!