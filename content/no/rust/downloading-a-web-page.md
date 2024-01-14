---
title:                "Rust: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Rust er et programmeringsspråk som har tatt utviklerverdenen med storm de siste årene. Med sin fokus på ytelse, sikkerhet og effektivitet, har Rust blitt et populært valg for utviklere som ønsker å bygge pålitelige og robuste softwaresystemer. I dette blogginnlegget skal vi ta en dypere titt på hvordan Rust kan brukes til å laste ned en nettside.

## Hvordan

For å laste ned en nettside i Rust, kan vi bruke biblioteket `reqwest`. Dette biblioteket gir muligheten til å gjøre HTTP-forespørsler og håndtere responsene på en enkel måte. La oss se på et eksempel på kode som laster ned og skriver ut HTML-koden til en nettside:

```
Rust
use reqwest::Client;

let client = Client::new();
let response = client.get("https://www.example.com").send().await?;
let html = response.text().await?;

println!("{}", html);
```

Koden ovenfor oppretter først en ny klient fra `reqwest::Client` og bruker deretter `get()`-metoden for å spesifisere nettadressen vi ønsker å laste ned. Deretter bruker vi metoden `send()` for å sende forespørselen og lagrer responsen i en variabel. Til slutt bruker vi `text()`-metoden for å få tilgang til HTML-koden og skriver den ut på skjermen.

## Dypdykk

Nå som vi har sett på et enkelt eksempel på hvordan vi kan laste ned en nettside i Rust, la oss se på noen tekniske detaljer. En av de viktigste egenskapene til `reqwest`-biblioteket er at det er asynkront, noe som betyr at det kan håndtere flere forespørsler samtidig. Dette gjør det spesielt godt egnet til nettside-krav som ofte innebærer flere HTTP-forespørsler.

I tillegg til dette, er `reqwest` også i stand til å håndtere sikre HTTPS-forespørsler og tilbyr flere avanserte funksjoner som å sette tilpassede header-informasjoner og håndtere redirectede forespørsler.

## Se også

- [offisiell Rust dokumentasjon for `reqwest`](https://docs.rs/reqwest/)
- [Rust for nybegynnere: en guide til å komme i gang](https://mnivik.github.io/rust-for-nybegynnere/)
- [Offisiell Rust-nettside](https://www.rust-lang.org/)