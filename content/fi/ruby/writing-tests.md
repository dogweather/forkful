---
title:    "Ruby: Testausten kirjoittaminen"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Testien kirjoittamiseen on monia syitä, mutta tärkein niistä on luottamuksen rakentaminen omaan koodiin. Testien avulla voit varmistaa, että koodisi toimii odotetulla tavalla ja vähentää mahdollisten virheiden riskiä. Lisäksi testien avulla on helpompi havaita ja korjata bugeja ennen kuin ne pääsevät tuotantoon.

## Miten

Testaaminen Rubyssa tapahtuu yleisimmin käyttämällä RSpec-kirjastoa. RSpec on testikirjasto, joka mahdollistaa selkeän ja helpon testien kirjoittamisen. Katso alla oleva esimerkki siitä, kuinka voit käyttää RSpec-kirjastoa testien kirjoittamiseen:

```Ruby
# Esimerkki testiluokasta
describe Calculator do
  # Esimerkki testin nimestä ja toiminnasta
  it "palauttaa oikean summan" do
    # Alustetaan muuttujat
    numero1 = 5
    numero2 = 10
    # Kutsutaan funktiota
    summa = Calculator.summaa(numero1, numero2)
    # Vertaillaan funktiosta saatua tulosta odotettuun
    expect(summa).to eq(15)
  end
end
```

Testien kirjoittamisessa kannattaa noudattaa DRY-periaatetta eli välttää turhien samanlaisten testien kirjoittamista. Lisäksi testien tulee olla erillään muusta koodista, jotta ne eivät vaikuta koodin toimintaan.

## Syvällinen sukellus

Testien kirjoittamisessa on tärkeää miettiä, mitä haluat testata ja mitä haluat jättää testien ulkopuolelle. Testien tulee kattaa tärkeimmät osat koodista ja niiden tulee olla selkeitä ja ymmärrettäviä. On myös tärkeää muistaa päivittää testejä, kun teet muutoksia koodiin.

Lisäksi testien kirjoittamisessa kannattaa hyödyntää myös muita kirjastoja, kuten FactoryGirlia, joka mahdollistaa testidatan luomisen helpommin. Muista myös, että testien kirjoittaminen on jatkuva prosessi ja niitä kannattaa päivittää ja parannella ajan myötä.

## Katso myös

- [RSpec-kirjaston kotisivu](http://rspec.info/)
- [FactoryGirl-kirjaston dokumentaatio](http://www.rubydoc.info/gems/factory_girl/file/GETTING_STARTED.md)
- [The RSpec Book](https://pragprog.com/book/achbd/the-rspec-book)