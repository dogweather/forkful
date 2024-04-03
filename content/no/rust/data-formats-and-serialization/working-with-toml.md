---
date: 2024-01-26 04:26:13.785024-07:00
description: 'Hvordan: .'
lastmod: '2024-03-13T22:44:40.597364-06:00'
model: gpt-4-0125-preview
summary: .
title: Jobbe med TOML
weight: 39
---

## Hvordan:
```Rust
// 1. Inkluder 'toml'-pakken i din Cargo.toml
// [dependencies]
// toml = "0.5"

// 2. Deserialiser TOML til en struct i Rust
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
    
    println!("Serveren kjører på {}:{}", host, port);
    // Utdata: Serveren kjører på "localhost":8080
}
```

## Dybde Dykking
TOML, som står for Toms Opplysende, Minimalistiske Språk, ble skapt av Tom Preston-Werner i 2013. Det tar sikte på å være mer leselig enn JSON eller YAML for konfigurasjonsfiler. TOMLs design fokuserer på utvetydig syntaks, minimalisme, og enkel kartlegging til datatyper.

Alternativer til TOML inkluderer JSON, YAML, og XML, men TOML vinner i scenarioer der menneskelig lesbarhet og filredigering av ikke-programmerere er avgjørende. Når man jobber med TOML i Rust, gir serde et sterkt grunnlag for serialisering og deserialisering, ved å bruke trekk for å kartlegge TOML på Rusts strukturer uten anstrengelse.

En utfordring ved å jobbe med TOML er dens strenghet på typer og struktur. Programmereren må definere et godt strukturert Rust typsystem som reflekterer schemaet til TOML dataen for å effektivt utnytte TOML i Rust.

## Se Også
- [TOML Dokumentasjon](https://toml.io/en/)
- [serde_toml Pakke](https://docs.rs/serde_toml/)
- [Rust Programmeringsspråk Bok](https://doc.rust-lang.org/stable/book/)
- [TOML GitHub Repo](https://github.com/toml-lang/toml)
