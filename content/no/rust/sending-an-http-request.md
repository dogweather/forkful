---
title:                "Sender en http-forespørsel"
html_title:           "Rust: Sender en http-forespørsel"
simple_title:         "Sender en http-forespørsel"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Hvorfor

Å sende HTTP-forespørsler kan være en viktig del av å utvikle nettapplikasjoner og automatisere prosesser. Det kan hjelpe deg å kommunisere med eksterne tjenester og få tilgang til data eller ressurser som du trenger.

# Hvordan

Hvis du vil sende en HTTP-forespørsel i Rust, kan du bruke et bibliotek som heter "reqwest". Først må du legge til det i prosjektet ditt ved å legge til dette i din "Cargo.toml" fil:

```Rust
[dependencies]
reqwest = { version = "0.10", features = ["json"] }
```

Deretter kan du importere biblioteket i filen din:

```Rust
use reqwest;
```

For å sende en HTTP GET-forespørsel til en URL, kan du bruke følgende kode:

```Rust
let resp = reqwest::blocking::get("https://www.example.com")?
    .text()?;
println!("{:#?}", resp);
```

Dette vil sende en GET-forespørsel til den spesifiserte URL-en og skrive ut svaret som tekst. Du kan også utføre andre handlinger som å sende POST-forespørsler, sette tilpassede headere og behandle feilmeldinger ved hjelp av dette biblioteket.

# Dykk dypere

Det finnes forskjellige måter å sende HTTP-forespørsler på i Rust, og "reqwest" er bare ett alternativ. Hvis du er interessert i å se på andre biblioteker eller bygge din egen løsning, kan du lese mer om "hyper" biblioteket eller se på "actix-web" rammeverket for nettutvikling i Rust.

# Se også

- [reqwest dokumentasjon](https://docs.rs/reqwest)
- [hyper bibliotek](https://hyper.rs/)
- [actix-web rammeverk](https://actix.rs/)