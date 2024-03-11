---
date: 2024-01-26 04:26:22.278021-07:00
description: "TOML \xE4r ett m\xE4nniskol\xE4sbart data-serialiseringsspr\xE5k som\
  \ ofta anv\xE4nds f\xF6r konfigurationsfiler. Programmerare anv\xE4nder TOML f\xF6\
  r dess enkelhet och klarhet,\u2026"
lastmod: '2024-03-11T00:14:11.062841-06:00'
model: gpt-4-0125-preview
summary: "TOML \xE4r ett m\xE4nniskol\xE4sbart data-serialiseringsspr\xE5k som ofta\
  \ anv\xE4nds f\xF6r konfigurationsfiler. Programmerare anv\xE4nder TOML f\xF6r dess\
  \ enkelhet och klarhet,\u2026"
title: Att arbeta med TOML
---

{{< edit_this_page >}}

## Vad & Varför?
TOML är ett människoläsbart data-serialiseringsspråk som ofta används för konfigurationsfiler. Programmerare använder TOML för dess enkelhet och klarhet, vilket lätt översätts till en hash map i Rust.

## Hur man gör:
```Rust
// 1. Inkludera 'toml'-modulen i ditt Cargo.toml
// [beroenden]
// toml = "0.5"

// 2. Avserialisera TOML till en struktur i Rust
use toml::Value;

fn main() {
    let toml_content = r#"
        [server]
        host = "localhost"
        port = 8080
    "#;

    let value = toml_content.parse::<Value>().unwrap();
    let host = value.get("server").unwrap().get("host").unwrap();
    let port = value.get("server").unwrap().get("port").unwrap();
    
    println!("Servern körs på {}:{}", host, port);
    // Utdata: Servern körs på "localhost":8080
}
```

## Fördjupning
TOML, som står för Toms Uppenbara, Minimala Språk, skapades av Tom Preston-Werner år 2013. Det syftar till att vara mer läsbart än JSON eller YAML för konfigurationsfiler. TOML:s design fokuserar på entydig syntax, minimalism och enkel kartläggning till datatyper.

Alternativ till TOML inkluderar JSON, YAML och XML, men TOML vinner i scenarier där mänsklig läsbarhet och filredigering av icke-programmerare är avgörande. När man arbetar med TOML i Rust, erbjuder serde en stark grund för serialisering och avserialisering, och använder traits för att smidigt mappa TOML till Rusts strukturer.

En utmaning vid arbete med TOML är dess strikthet på typer och struktur. Programmeraren måste definiera ett välstrukturerat Rust-typsystem som återspeglar schemat för TOML-data för att effektivt utnyttja TOML i Rust.

## Se även
- [TOML Dokumentation](https://toml.io/en/)
- [serde_toml-modul](https://docs.rs/serde_toml/)
- [Rust programmeringsspråkets bok](https://doc.rust-lang.org/stable/book/)
- [TOML GitHub Repo](https://github.com/toml-lang/toml)
