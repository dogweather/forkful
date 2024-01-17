---
title:                "Testien kirjoittaminen"
html_title:           "Ruby: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/writing-tests.md"
---

{{< edit_this_page >}}

# Ruby-ohjelmoinnin testien kirjoittaminen

## Mitä ja miksi?

Testien kirjoittaminen on prosessi, jossa ohjelmoija varmistaa koodin toimivuuden ja virheettömyyden. Testauksen avulla voidaan simuloida erilaisia skenaarioita ja tarkistaa, että ohjelma toimii halutulla tavalla. Testien kirjoittaminen on tärkeä osa ohjelmistokehitystä, sillä se auttaa varmistamaan koodin laadun ja vähentämään virheiden määrää.

## Kuinka tehdä?

Testien kirjoittamiseen on useita eri menetelmiä, mutta yksi suosituimmista tavoista on käyttää RSpec-kirjastoa. RSpec on testauskirjasto, joka on suunniteltu erityisesti Ruby-ohjelmointikielelle. Se tarjoaa helpon ja selkeän tavan kirjoittaa testejä koodille.

Esimerkiksi, jos haluamme testata yksinkertaisen laskufunktion, voimme käyttää seuraavaa koodia RSpec-testissä:

```Ruby
def sum(num1, num2)
  return num1 + num2
end

RSpec.describe "sum" do
 it "returns the sum of two numbers" do
  expect(sum(3,4)).to eq(7)
 end
end
```

Tämän testin avulla varmistamme, että `sum`-funktio palauttaa oikean tuloksen, kun sille annetaan kaksi lukua.

## Tarkempi tarkastelu

Testien kirjoittamisen taustalla on ajatus testausvetoisesta kehityksestä (test-driven development), jossa testit kirjoitetaan ennen varsinaisen koodin luontia. Tällä tavalla ohjelmoija voi varmistaa, että koodi tekee halutun asian ja että mahdolliset muutokset eivät aiheuta ei-toivottuja sivuvaikutuksia.

Varsinaisen koodin testaamisen lisäksi on myös olemassa muita testauksen vaihtoehtoja, kuten manuaalinen tai automaattinen testaus. Manuaalinen testaus tarkoittaa, että joku testaa koodin käsin ja tarkistaa sen toimivuuden. Automaattisessa testauksessa taas käytetään ohjelmia tai skriptejä, joiden avulla testit suoritetaan automaattisesti.

## Katso myös

Jos haluat oppia lisää testien kirjoittamisesta Rubyssa, voit tutustua seuraaviin lähteisiin:

- [RSpec - Ruby-ohjelmistojen testauskirjasto](https://rspec.info/)
- [Testivetoiset kehitysmenetelmät](https://fi.wikipedia.org/wiki/Testivetoiset_kehitysmenetelm%C3%A4t)
- [Testausvetoisen kehityksen (TDD) perusteet Rubyssa](https://www.rubyguides.com/2016/07/rspec-tdd-in-ruby/)