---
title:    "Rust: Merkkijonojen yhdistäminen"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi voit haluta yhdistää merkkijonoja Rust-ohjelmoinnissa. Se voi olla hyödyllistä kun luodaan käyttäjälle loppukäyttäjälle näkyvä käyttöliittymä tai tallennetaan tietoja tietokantaan.

## Kuinka tehdä

Rustissa, merkkijonojen yhdistäminen käyttäen `+` operaattoria on yksinkertaista. Voit yhdistää kaksi merkkijonoa yksinkertaisesti kirjoittamalla ne peräkkäin. Esimerkiksi:

```Rust
let etunimi = "Matti";
let sukunimi = "Meikäläinen";

let kokonimi = etunimi + sukunimi;

println!("{}", kokonimi);
```

Tämä tulostaisi `Matti Meikäläinen`. On myös tärkeää huomata, että yhdistäminen luo kokonaan uuden merkkijonon, eikä muuta olemassaolevia.

## Syvällinen kuvaus

Rustissa, merkkijonat ovat itse asiassa siirrettäviä osia muistista, mikä tarkoittaa, että ne voivat olla hieman monimutkaisempia käsitellä ja yhdistää kuin muut datatyypit. Kuitenkin, käyttämällä `+` operaattoria yhdistäessä, Rust käyttää automaattisesti muistinhallintaa varmistaakseen että merkkijonat pysyvät eheinä ja oikein muodostettuina.

## Katso myös

- [Rustin opas merkkijonojen käsittelyyn](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Rustin virallinen verkkosivusto](https://www.rust-lang.org/fi/)
- [Rustin muistinhallintamalleista](https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html)