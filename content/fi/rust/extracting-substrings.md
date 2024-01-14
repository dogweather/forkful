---
title:                "Rust: Alimerkkijonojen erottelu"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

Rust: Mitä substrings ovat ja miten niitä käytetään

## Miksi

On olemassa monia tilanteita, joissa ohjelmoinnissa haluamme käsitellä osia merkkijonosta sen sijaan, että käsittelemme sitä kokonaisuutena. Tämä voi olla tarpeen esimerkiksi tietojen analysoinnissa tai tietyn tiedon etsimisessä tietokannoista. Rustin substring-toiminnallisuus tarjoaa mahdollisuuden käsitellä merkkijonon osia tehokkaasti ja luotettavasti.

## Kuinka

Rustin String-tyyppi tarjoaa valmiina metodin nimeltä `slice` jota voidaan käyttää merkkijonon osien erottamiseen. Tämä toimii samalla tavalla kuin indeksointi tavallisessa taulukossa. Käytännössä voimme antaa `slice` metodille halutun merkkijonon indeksin alusta ja lopusta, ja se palauttaa meille uuden merkkijonon, joka sisältää vain valitut osat.

```Rust
fn main() {
    let s = "Tämä on esimerkkimerkkijono";

    //erota merkkijonon osa indekseillä
    let sliced = &s[5..12];

    println!("{}", sliced); //tulostaa "on esim"
}
```

Kuten yllä olevasta koodista näemme, `slice`-metodia käytetään String-tyypin muuttujan nimen jälkeen hakasulkeilla, joissa ilmoitetaan haluttu alku- ja loppuindeksi. Indeksin loppuindeksi ei sisälly uuteen merkkijonoon, joten se on kellonaikojen välillä [alku, loppu).

## Syvällinen sukellus

Substringsit ovat sisäisesti esitettyjä String-slicejä, jotka ovat ikään kuin lainattuja suoraan alkuperäisestä merkkijonosta eivätkä vie ylimääräistä muistia. Tämä tekee niistä tehokkaan käytön erilaisissa ohjelmointitehtävissä. Substringsit ovat myös muokattavissa, joten voimme esimerkiksi käyttää niitä muuttamaan alkuperäistä merkkijonoa.

Lisäksi Rust tarjoaa muita hyödyllisiä metodeja, kuten `split` ja `splitn`, jotka voivat jakaa merkkijonon osiin halutun välimerkin perusteella. Nämä metodit palauttavat iteraattoreita, jotta voimme käsitellä erikseen jokaisen palan merkkijonosta.

## Katso myös

- [Rustin virallinen dokumentaatio substrings](https://doc.rust-lang.org/std/primitive.str.html#method.slice)
- [Tutorialspoint: Merkkijonon manipulointi Rustilla](https://www.tutorialspoint.com/rust/rust_strings.htm)
- [Laaja lista Rustin String-tyypin metodeista](https://doc.rust-lang.org/std/string/struct.String.html#method.slice)

Kiitos lukemisesta ja onnea substringien käyttöön!