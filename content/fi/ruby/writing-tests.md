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

## Miksi

On tärkeää kirjoittaa testejä ohjelmointikoodin yhteydessä, jotta ohjelmistojen laatu voidaan varmistaa ja mahdolliset virheet voidaan havaita ja korjata varhaisessa vaiheessa. Testit myös helpottavat koodin ymmärtämistä ja muokkaamista tulevaisuudessa.

## Miten

Testien kirjoittaminen Rubyllä on helppoa ja siinä käytetään usein kirjastoa nimeltä RSpec. Alla on esimerkkejä yksikkötestien ja integraatiotestien kirjoittamisesta:

```Ruby
require 'rspec'
require_relative 'calculator'

describe Calculator do
  # Testataan yhteenlaskun toimivuus
  it 'laskee kaksi lukua yhteen' do
    expect(Calculator.add(3, 5)).to eql(8)
  end
end

describe IntegrationTest do
  # Testataan, että laskenta oikeasti käyttää laskuria
  it 'käyttää oikeaa laskuria' do
    expect(IntegrationTest.new.run).to eql(true)
  end
end
```

Tämän koodin avulla voimme varmistaa, että oma laskurimme toimii halutulla tavalla. Huomaathan, että kutsumme add-metodia suoraan Calculator-luokasta ja kutsutaan myös run-metodia IntegrationTest-luokasta.

## Syväsukellus

Testien kirjoittaminen tulee tehdä huolellisesti ja kattavasti. Testien tulisi kattaa kaikki mahdolliset tapaukset ja skenaariot, jotta voidaan varmistua koodin oikeellisuudesta. On myös tärkeää tehdä testauksesta osa ohjelmointiprosessia heti alusta alkaen, jotta vältetään mahdolliset virheet ja ongelmakohdat.

## Katso myös

- [RSpecin dokumentaatio](https://rspec.info/documentation/)
- [Unit-testauksen perusteet Rubylla](https://www.rubyguides.com/2018/07/rspec-tutorial/)
- [Testauksen tärkeys ohjelmoinnissa](https://medium.com/microtica/why-unit-testing-is-important-in-software-development-a457355d5225)