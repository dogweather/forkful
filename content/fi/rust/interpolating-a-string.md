---
title:                "Yhdistävän merkkijonon interpolointi"
html_title:           "Rust: Yhdistävän merkkijonon interpolointi"
simple_title:         "Yhdistävän merkkijonon interpolointi"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Stringien interpolointi on menetelmä, joka mahdollistaa muuttujien arvojen sijoittamisen tekstinsisään. Tämä helpottaa ohjelmoinnin tekoa ja tekee koodista luettavampaa.

## Näin teet sen:
Käytä ```Rust-tiedostossa println! ``` -funktio tai ```format! ``` -makroa sijoittaaksesi muuttujien arvot tekstiin haluttuihin kohtiin. Esimerkiksi:
```Rust
let nimi = "Maija";
println!("Hei, minun nimeni on {}.", nimi);
```
Tulostus näyttää tältä:
```
Hei, minun nimeni on Maija.
```

Voit myös lisätä useampia muuttujia peräkkäin sijoittamalla ne indeksin avulla:
```Rust
let nimi = "Maija";
let ikä = 30;
println!("Hei, minun nimeni on {} ja olen {} vuotta vanha.", nimi, ikä);
```
Tulostus näyttää tältä:
```
Hei, minun nimeni on Maija ja olen 30 vuotta vanha.
```

## Syvemmälle:
Stringien interpolointi tuli ensimmäisen kerran käyttöön Perl-ohjelmointikielessä ja on sittemmin tullut suosituksi myös muissa ohjelmointikielissä, kuten Pythonissa ja JavaScriptissä. Toinen tapa sijoittaa muuttujia tekstiin on käyttää pienoiskomennoja, mutta tämä ei ole yhtä helppokäyttöinen kuin interpolointi.

Rustin format! -makro yhdistää tyylikkäästi tekstiä ja muuttujia, ja se on myös turvallisempi vaihtoehto kuin perinteinen muuttujien sijoittaminen merkkijonomuuttujien avulla. Makro varmistaa, että teksti on oikein muotoiltu eikä aiheuta päästöongelmia.

## Katso myös:
- Rustin virallinen dokumentaatio: https://doc.rust-lang.org/std/fmt/
- String interpolation esimerkkejä: https://doc.rust-lang.org/stable/rust-by-example/hello/print/print_debug.html