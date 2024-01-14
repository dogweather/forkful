---
title:    "Gleam: Testien kirjoittaminen"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Miksi kirjoittaa testejä?

Testaaminen on tärkeä osa ohjelmointia ja auttaa varmistamaan, että koodi toimii odotetulla tavalla. Kirjoittamalla testejä voit vähentää virheiden määrää ja parantaa ohjelman luotettavuutta. 

# Kuinka kirjoittaa testeja Gleamilla?

Käyttämällä Gleamin testausmoduulia voit helposti kirjoittaa yksikkötestejä ja integraatiotestejä. Alla on esimerkkejä koodista ja niiden odotetusta tulosteesta. 

```Gleam
// yksinkertainen funktio kertomaan kahdesta luvusta
pub fn kerro(a: Int, b: Int) -> Int {
    a * b
}

// testi, joka tarkistaa, että funktio toimii oikein
test "kerro-palauttaa oikean tuloksen" {
    expect(kerro(2, 3)).to_equal(6)
}

// odotettu tulos: Test passed!
```

```Gleam
// funktio tarkistamaan, onko luku parillinen
pub fn onko_parillinen(x: Int) -> Bool {
    x % 2 == 0
}

// testi, joka tarkistaa, että funktio toimii oikein
test "onko_parillinen-palauttaa_oikean_tuloksen" {
    expect(onko_parillinen(4)).to_equal(true)
}

// odotettu tulos: Test passed!
```

# Syvemmälle testien kirjoittamiseen

Testien kirjoittaminen ei rajoitu vain yksinkertaisiin esimerkkeihin, vaan voit myös testata monimutkaisempia funktioita ja luokkia. Gleamin Testing -moduulin dokumentaatio tarjoaa lisää tietoa eri testausfunktioista ja -ominaisuuksista. 

# Katso myös

- Gleamin Testing -moduulin dokumentaatio: [https://gleam.run/documentation/docs/tests](https://gleam.run/documentation/docs/tests)
- Gleamin virallinen verkkosivusto: [https://gleam.run/](https://gleam.run/)
- Gleamin GitHub-arkisto: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)