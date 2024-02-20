---
date: 2024-01-26 04:10:11.665004-07:00
description: "Debuggerin k\xE4ytt\xE4minen on kuin antaisi itsellesi r\xF6ntgenn\xE4\
  \xF6n kurkistaa koodisi suoritukseen. Ohjelmoijat tekev\xE4t sen bongatakseen bugeja,\
  \ ymm\xE4rt\xE4\xE4kseen\u2026"
lastmod: 2024-02-19 22:05:15.256417
model: gpt-4-0125-preview
summary: "Debuggerin k\xE4ytt\xE4minen on kuin antaisi itsellesi r\xF6ntgenn\xE4\xF6\
  n kurkistaa koodisi suoritukseen. Ohjelmoijat tekev\xE4t sen bongatakseen bugeja,\
  \ ymm\xE4rt\xE4\xE4kseen\u2026"
title: "Debuggerin k\xE4ytt\xF6"
---

{{< edit_this_page >}}

## Mikä ja miksi?

Debuggerin käyttäminen on kuin antaisi itsellesi röntgennäön kurkistaa koodisi suoritukseen. Ohjelmoijat tekevät sen bongatakseen bugeja, ymmärtääkseen ohjelman kulun ja varmistaakseen, että heidän koodinsa on puhdas kuin pilli. Se on kuin olisi kaveri, joka osoittaa tarkalleen, missä kompastuit.

## Kuinka:

Rust tukee useita debuggereita, mutta yleinen on `gdb` GNU/Linuxille tai `lldb` macOS:lle. Saatat myös käyttää `rust-gdb`:tä tai `rust-lldb`:tä, jotka ovat kääreitä, jotka tulostavat Rust-arvot kauniisti. Tässä esimerkki:

```Rust
fn main() {
    let mut laskuri = 0;
    for _ in 0..5 {
        laskuri += 1;
        println!("Laskuri on: {}", laskuri);
    }
}
```

Debugataksesi tätä, käännä debug-tiedot mukana:

```shell
$ rustc -g laskuri.rs
```

Sitten aja se `rust-gdb`:ssä:

```shell
$ rust-gdb laskuri
(gdb) break main
(gdb) run
(gdb) print laskuri
$1 = 0
(gdb) continue
Laskuri on: 1
(gdb) print laskuri
$2 = 1
```

## Syväluotaus

Debuggaus on ollut olemassa jo *vanhoista hyvistä ajoista* lähtien, kun käytettiin reikäkortteja, ja sen kehitys on ollut Jumalan lahja. Rust tarjoaa omat työkalunsa GDB:n ja LLDB:n integraatioilla johtuen kielen järjestelmätason luonteesta.

Vaihtoehtoja Rust-koodin debuggaukselle sisältävät integroitujen kehitysympäristöjen (IDE) käytön niiden sisäänrakennettujen debuggerien kanssa, jotka jotkut löytävät intuitiivisemmiksi. Suosittuja ovat CLion Rust-lisäosan kanssa tai Visual Studio Code Rust-laajennuksen kanssa.

Toteutuksen osalta Rust tuottaa debug-symboleita, joita nämä debuggerit ymmärtävät, mikä on elintärkeää koodin läpi kulkemisessa, katkaisupisteiden asettamisessa ja muuttujien tarkastelussa menettämättä järkeäsi.

## Katso myös

- Rust-kirja Debuggauksesta: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#guidelines-for-error-handling
- Rust By Example:n näkemys Virheistä ja Debuggauksesta: https://doc.rust-lang.org/rust-by-example/error.html
- The Rust Language Server (RLS), joka tekee mahdolliseksi VS Code:n Rust-laajennuksen: https://github.com/rust-lang/rls
- Rustin debuggaus Visual Studio Codella: https://marketplace.visualstudio.com/items?itemName=rust-lang.rust
