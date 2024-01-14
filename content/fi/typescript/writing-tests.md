---
title:    "TypeScript: Testien kirjoittaminen"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Miksi

Testien kirjoittaminen on tärkeä osa koodauksen prosessia, joka varmistaa, että ohjelmisto toimii odotetulla tavalla. Testien avulla voidaan vähentää ohjelmointivirheitä ja parantaa ohjelmiston laatua. 

## Kuinka tehdä

Kun käytetään TypeScriptiä ohjelmistokehityksessä, testien kirjoittaminen on suositeltavaa. Tässä on esimerkki testin kirjoittamisesta, jossa tarkistetaan, että annetut syötteet tuottavat odotetun tuloksen:

```TypeScript
// Testattava funktio
function sum(x: number, y: number): number {
  return x + y;
}

// Testin kirjoittaminen
function testSum() {
  let result = sum(3, 5);
  if (result === 8) {
    console.log("Testi läpäisty");
  } else {
    console.log("Testi epäonnistui");
  }
}

// Testin suorittaminen
testSum();

// Tulostus: Testi läpäisty
```

Tässä esimerkissä testataan `sum`-funktiota, joka laskee kahden numeron summan. Testin avulla varmistetaan, että funktio toimii odotetulla tavalla ja palauttaa oikean tuloksen.

## Syvemmälle testien kirjoittamiseen

Testien kirjoittaminen voi tuntua työläältä, mutta se tuo monia etuja. Kattavampien testien avulla voidaan vähentää ohjelmointivirheitä ja parantaa ohjelmiston luotettavuutta. Testien avulla voidaan myös helpommin löytää ja korjata mahdollisia ongelmia ohjelmassa.

Testien kirjoittamiseen on monia eri lähestymistapoja ja työkaluja, kuten esimerkiksi Jest ja Jasmine. On tärkeää löytää itselleen sopiva tapa testien kirjoittamiseen ja pitää niiden määrä järkevänä.

### Hyviä käytäntöjä testien kirjoittamiseen

- Testaa eri syötteillä ja reunatapauksilla. Mitä enemmän testejä, sitä paremmin ne kattavat koodin.
- Testaa vain yhtä asiaa kerrallaan. Näin on helpompi tunnistaa, missä kohdassa on mahdollinen ongelma.
- Nimeä testit kuvaavasti, jotta on helppo nähdä mitä ne testaavat.
- Pidä testit mahdollisimman yksinkertaisina ja helppolukuisina.

## Katso myös

- [TypeScriptin virallinen ohjeistus testaamisesta](https://www.typescriptlang.org/docs/handbook/testing.html)
- [Jest-dokumentaatio](https://jestjs.io/docs/en/getting-started)
- [Jasmine-dokumentaatio](https://jasmine.github.io/tutorials/your_first_suite)