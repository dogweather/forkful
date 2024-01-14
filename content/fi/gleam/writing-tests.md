---
title:    "Gleam: Testien kirjoittaminen"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Yksi tärkeimmistä osa-alueista ohjelmistokehityksessä on testaaminen. Testien kirjoittaminen auttaa varmistamaan, että koodi toimii oikein ja vähentää mahdollisten virheiden riskiä tuotantokäytössä.

## Kuinka

Testien kirjoittaminen Gleam-kielellä on helppoa ja intuitiivista. Alla on esimerkkejä koodista ja sen tulosteista, jotta voit aloittaa oman testikoodisi kirjoittamisen.

```Gleam
test "summaa kaksi numeroa" {
  expect 5 |> equal(summaa(2, 3))
}
```

Tässä testissä määrittelemme funktion `summaa`, joka laskee kahden numeron summan ja varmistamme, että sen tuloksena on odotettu arvo. Voit määritellä useita testejä samassa tiedostossa ja ajaa ne kaikki yhdellä komennolla.

```
$ gleam test
```

### Tulostus

```
Running 1 test module

summaa kaksi numeroa
✓ Pass

1/1 tests passed.

🎉 All tests passed! 🎉
```

Voit myös halutessasi ajaa testit automaattisesti jokaisen uuden koodimuutoksen jälkeen. Tässä esimerkissä käytämme `fswatch`-työkalua automaattisen testauksen mahdollistamiseksi.

```
$ fswatch -o . | xargs -n1 -I{} gleam test
```

## Syvemmälle

Voit joustavasti määritellä testauksessa käytettäviä fakseja, odotuksia ja muita ominaisuuksia. Gleam tarjoaa myös `skip`-funktion, joka mahdollistaa testien ohittamisen tarpeen mukaan.

```Gleam
test "skippaa tämä testi" {
  skip
  expect 2 |> equal(summaa(1, 2))
}
```

Voit myös käyttää `group`-funktiota ryhmittelyn avulla, mikä helpottaa useiden testien hallintaa ja ajamista yhtenä kokonaisuutena.

```Gleam
group "summaFunktion testit" {
  test "palauttaa oikean tuloksen" {
    expect 10 |> equal(summaa(3, 7))
  }

  test "ei aiheuta virhettä, vaikka tulosten määrä on suuri" {
    let tulos = summaa(999, 999)
    nil |> equal(tulos)
  }
}
```

## Katso myös

- [Gleam-dokumentaatio](https://gleam.run/book/)
- [Gleam-yhteisön maksuton Slack-kanava](https://gleam-lang.slack.com/)
- [Gleam-testikirjasto](https://github.com/gleam-lang/gleam_testing)