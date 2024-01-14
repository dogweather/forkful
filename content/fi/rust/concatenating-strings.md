---
title:                "Rust: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

# Miksi Rustissa kannattaa käyttää stringien yhdistämistä?

## Miksi

Rust on nopeasti kasvava ohjelmointikieli, joka tunnetaan sen tehokkuudesta ja turvallisuudesta. Yhdistäminen eli stringien liittäminen toisiinsa on yleinen tehtävä ohjelmoinnissa, ja Rustissa on useita tapoja suorittaa se. Tässä blogikirjoituksessa käymme läpi, miksi ja miten voit käyttää Rustissa stringien yhdistämistä.

## Kuinka tehdä

Rustin standardeissa kirjastoissa on useita toimintoja stringien yhdistämiseen, mukaan lukien `format!` ja `join` metodit. Voit myös käyttää `+` operaattoria yhdistääksesi kaksi stringiä. Alla on esimerkki kahden stringin yhdistämisestä käyttäen `format!` ja `+`:

```Rust
let etunimi = "Matti";
let sukunimi = "Meikäläinen";
let kokonimi = format!("{} {}", etunimi, sukunimi);
let kokonimi2 = etunimi + " " + &sukunimi;
println!("Kokonimi: {}", kokonimi);
println!("Kokonimi 2: {}", kokonimi2);
```

Tulostus:

```
Kokonimi: Matti Meikäläinen
Kokonimi 2: Matti Meikäläinen
```

Kuten näemme, molemmat tavat tuottavat saman tuloksen. `format!` palauttaa uuden stringin, kun taas `+` operaattori yhdistää stringit suoraan.

Rustissa voit myös käyttää `join` metodia yhdistämään useita stringejä yhdeksi stringiksi. Tämä metodi ottaa vektorin tai iteraattorin stringeistä ja yhdistää ne annetulla välimerkillä. Alla on esimerkki `join` metodin käytöstä:

```Rust
let vihannekset = ["tomaatti", "kurkku", "paprika"];
let vihannekset_stringiksi = vihannekset.join(", ");
println!("Vihannekset: {}", vihannekset_stringiksi);
```

Tulostus:

```
Vihannekset: tomaatti, kurkku, paprika
```

## Syvempi sukellus

Rustissa `+` operaattorin käyttäminen yhdistämiseen voi aiheuttaa omistajuusongelmia. Esimerkiksi jos yrität yhdistää kaksi muuttujaa, joista toinen on muuttumaton (immutable) ja toinen muuttuva (mutable), niin Rust aiheuttaa virheen. Tämä johtuu siitä, että `+` operaattori ottaa omistajuuden molemmista muuttujista ja yrittää yhdistää niitä, mutta muuttumaton muuttuja ei voi luovuttaa omistajuutta ja Rustissa ei sallita muutettavien ja muuttumattomien muuttujien yhdistämistä.

Toinen tärkeä huomioitava asia on, että Rustissa stringit eivät ole muutettavissa (immutable) oletuksena. Tämä tarkoittaa sitä, että et voi muuttaa jo olemassa olevaa stringiä, vaan joudut luomaan uuden joka kerta kun teet muutoksia. Tämä pätee myös yhdistämiseen, joten vaikka `+` operaattori vaikuttaisi yhdistävän stringejä, se todellisuudessa luo uuden stringin joka kerta. Tästä syystä `join` metodi on usein tehokkaampi vaihtoehto, kun yhdistät useita stringejä.

## Katso myös

- [Rustin oppaat ja esimerkit](https://doc.rust-lang.org/stable/rust-by-example/index.html