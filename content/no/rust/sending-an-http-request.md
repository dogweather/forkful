---
title:                "Rust: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

Rust er et kraftig programmeringsspråk som har blitt stadig mer populært blant utviklere. En av de mange grunnene til dette er dets evne til å lage sikker og effektiv kode. Og en av de tingene du ofte må gjøre i programmeringen din er å sende HTTP-forespørsler. Hvorfor bry seg med å lære hvordan man gjør dette i Rust? Vel, ved å bruke Rust kan du dra nytte av dets sikkerhet og effektivitet til å opprette pålitelige og raske webapplikasjoner.

## Hvordan

La oss se på hvordan du kan sende en HTTP-forespørsel ved hjelp av Rust. Først trenger vi å bruke et par biblioteker. I eksempelet vårt vil vi bruke "reqwest" og "tokio". Så la oss importere dem ved hjelp av "use"-uttrykket:

```Rust
use reqwest::Client;
use tokio::io::{AsyncBufReadExt, BufReader};
```

Nå er vi klare til å opprette en ny klient og sende en GET-forespørsel til en URL. Vi kan gjøre dette ved hjelp av kode i følgende form:

```Rust
let client = Client::new();
let response = client.get("https://www.example.com/")
    .send().await?
    .text().await?;
println!("Response body: {}", response);
```

Her oppretter vi først en ny klient med "Client::new()" metoden. Deretter sender vi en GET-forespørsel til en URL ved hjelp av "get" metoden og "send" funksjonen. Denne "await?" er nødvendig for å vente på et asynkront svar fra serveren. Til slutt får vi teksten fra responsen ved hjelp av "text" metoden.

Hvis vi trykker denne koden, vil vi få responsen tilbake som en streng, og deretter vises det i konsollen. Nå kan vi manipulere og bruke svaret som vi vil.

## Dykk ned

I dette eksempelet brukte vi bare noen grunnleggende funksjoner for å sende en HTTP-forespørsel. Men det er mye mer som kan utforskes. For eksempel kan vi legge til en "header" til forespørselen vår ved hjelp av "header" metoden og få svaret tilbake som en JSON i stedet for bare en streng ved å bruke "json" metoden. Det er også verdt å se på feilhåndtering og måter å optimalisere koden din på for å gjøre den enda mer effektiv.

## Se også

- Rust offisielle hjemmeside: https://www.rust-lang.org/no
- Offisiell dokumentasjon for reqwest: https://docs.rs/reqwest
- Offisiell dokumentasjon for tokio: https://tokio.rs