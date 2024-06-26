---
date: 2024-01-26 01:09:36.044457-07:00
description: "Miten: Aloitetaan peruslokitus skenaarion pystytt\xE4minen Rustissa\
  \ k\xE4ytt\xE4en `log`-kirstua, joka tarjoaa lokitusfasaadin, ja `env_logger`,\u2026"
lastmod: '2024-03-13T22:44:56.363181-06:00'
model: gpt-4-1106-preview
summary: "Aloitetaan peruslokitus skenaarion pystytt\xE4minen Rustissa k\xE4ytt\xE4\
  en `log`-kirstua, joka tarjoaa lokitusfasaadin, ja `env_logger`, lokitusimplementaation\
  \ `log`-kirstulle."
title: Lokitus
weight: 17
---

## Miten:
Aloitetaan peruslokitus skenaarion pystyttäminen Rustissa käyttäen `log`-kirstua, joka tarjoaa lokitusfasaadin, ja `env_logger`, lokitusimplementaation `log`-kirstulle. Aluksi, lisää ne Cargo.toml-tiedostoosi:

```toml
[dependencies]
log = "0.4.14"
env_logger = "0.9.0"
```

Nyt, pystytä ja alusta lokitus `main.rs`-tiedostossasi:

```rust
use log::{info, warn};

fn main() {
    env_logger::init();

    info!("Tämä on infotason viesti.");
    warn!("Tämä on varoitusviesti.");
}
```

Aja sovelluksesi komennolla `RUST_LOG=info cargo run`, ja näet tulosteen:

```
INFO: Tämä on infotason viesti.
WARN: Tämä on varoitusviesti.
```

Kokeile eri `RUST_LOG`-ympäristömuuttujan arvoja asettamalla se `error`, `warn`, `info`, `debug` tai `trace` säätääksesi lokisi yksityiskohtaisuutta.

## Syväsukellus
Lokituksen konsepti ei ole mitään uutta; se on ollut olemassa tietokoneiden alkuaikojen jälkeen. Ennen kuin lokitus oli yleistä ohjelmistossa, kehittäjät turvaantuivat alkeellisiin menetelmiin kuten tulostuslauseisiin tai debuggerityökaluihin ohjelman suorituksen jäljittämiseen. Ohjelmien monimutkaistuessa monimutkaistui myös tarve jäsennellyille lokituslähestymistavoille.

Rustissa `log`-kirstu abstrahoi lokitusyksityiskohdat, mahdollistaen kehittäjien liittää erilaisia lokitusbackendejä. Vaikka `env_logger` on yleinen valinta, on olemassa vaihtoehtoja kuten `fern`, `slog` tai `tracing`, joissa jokaisessa on oma ominaisuuksiensa ja konfiguraatiovaihtoehtojensa sarja.

Jotkin pohdittavat asiat lokituksen toteutuksessa sisältävät:

1. **Lokitason**: Yksityiskohtaisuuden hallinta on avainasemassa. Rustin `log`-kirstu määrittelee useita lokitasoja: error, warn, info, debug ja trace, vakavuuden vähentyessä järjestyksessä.

2. **Suorituskyky**: Lokitus voi vaikuttaa suorituskykyyn. Kriittistä on käyttää sitä harkiten, varmistaen että vältetään lokitusta suorituskyvyltään kriittisillä poluilla tai liian yksityiskohtaisia lokeja tuotannossa.

3. **Rakenteellinen Lokitus**: Nykyaikaiset parhaat käytännöt sisältävät rakenteellisen lokituksen, missä lokit kirjoitetaan koneellisesti luettavaan muotoon kuten JSON. Kirjastot kuten `slog` mahdollistavat rakenteellisen lokituksen Rustissa, jota voidaan indeksoida ja kysellä käyttäen lokinhallintajärjestelmiä kuten ELK Stack tai Splunk.

4. **Asynkroninen Lokitus**: Pääsovelluksen vaikutusten minimoimiseksi lokitus voidaan suorittaa asynkronisesti. Tämä saavutetaan usein antamalla lokituskirjaston kirjoittaa muistissa olevaan jonoon, ja erillinen säie käsittelee jonoa ja kirjoittaa lokit määränpäähän.

5. **Konfiguraatio**: Monet lokituskehykset tukevat konfiguraatiota ympäristömuuttujien, konfiguraatiotiedostojen ja/tai koodin kautta. Tämä joustavuus on avainasemassa tulostuksen hienosäätöön erilaisissa ympäristöissä (kehitys, staging, tuotanto).

## Katso Myös
- `log`-kirstun dokumentaatio: https://docs.rs/log/
- `env_logger`-kirstun dokumentaatio: https://docs.rs/env_logger/
- Rust by Example lokitus sivu: https://doc.rust-lang.org/rust-by-example/std_misc/log.html
- `slog`-kirstu, vaihtoehtoinen lokituskehys: https://github.com/slog-rs/slog
- Tracing, kehys Rust-ohjelmien instrumentointiin: https://crates.io/crates/tracing
