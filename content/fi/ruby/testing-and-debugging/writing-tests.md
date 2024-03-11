---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:14.156097-07:00
description: "Testaaminen Rubylla tarkoittaa koodisi toiminnan varmistamista odotetulla\
  \ tavalla erilaisissa olosuhteissa. Ohjelmoijat kirjoittavat testej\xE4\u2026"
lastmod: '2024-03-11T00:14:31.131731-06:00'
model: gpt-4-0125-preview
summary: "Testaaminen Rubylla tarkoittaa koodisi toiminnan varmistamista odotetulla\
  \ tavalla erilaisissa olosuhteissa. Ohjelmoijat kirjoittavat testej\xE4\u2026"
title: Testien kirjoittaminen
---

{{< edit_this_page >}}

## Mikä & Miksi?
Testaaminen Rubylla tarkoittaa koodisi toiminnan varmistamista odotetulla tavalla erilaisissa olosuhteissa. Ohjelmoijat kirjoittavat testejä varmistaakseen oikeellisuuden, estääkseen regressiot ja helpottaakseen refaktorointia, tähdäten vankkoihin ja ylläpidettäviin sovelluksiin.

## Miten:
Ruby sisältää valmiiksi `Test::Unit`-kirjaston yksikkötestien kirjoittamiseen, kapseloiden testikäytäntöjä yksinkertaisiin rakenteisiin. Kuitenkin Rubyn yhteisö kallistuu usein kolmannen osapuolen kirjastoihin, kuten RSpec ja Minitest, niiden paremman ilmaisukyvyn ja joustavuuden vuoksi.

### `Test::Unit`in käyttö:
```ruby
require 'test/unit'

class CalculatorTest < Test::Unit::TestCase
  def test_addition
    result = 2 + 2
    assert_equal 4, result
  end
end
```
Aja testitiedostosi komentoriviltä, ja sinun pitäisi saada tuloste, joka ilmoittaa testien onnistumisesta tai epäonnistumisesta:
```
Loaded suite test_calculator
Started
.
Valmis 0.001288 sekunnissa.
1 testit, 1 väitteet, 0 epäonnistumiset, 0 virheet, 0 odottavat, 0 puutteet, 0 ilmoitukset
100% läpäissyt
```

### RSpecin käyttö:
RSpec on suosittu BDD (Behavior-Driven Development) kehys Rubyille. Asenna gem komennolla `gem install rspec`, sitten alusta se projektissasi komennolla `rspec --init`.

```ruby
# calculator_spec.rb
require_relative '../calculator'

describe Calculator do
  it 'laskee kaksi lukua oikein' do
    expect(Calculator.add(2, 2)).to eq(4)
  end
end
```
Aja testit `rspec`-komennolla. Esimerkkituloste:
```
.

Valmis 0.002 sekunnissa (tiedostot ladattiin 0.1 sekunnissa)
1 esimerkki, 0 epäonnistumisia
```

### Minitestin käyttö:
Minitest tarjoaa täydellisen joukon testausmahdollisuuksia tukien TDD:tä, BDD:tä, väärentämistä ja suorituskyvyn testausta. Asenna se komennolla `gem install minitest` ja käytä seuraavasti:

```ruby
# test_calculator.rb
require 'minitest/autorun'
require_relative '../calculator'

class CalculatorTest < Minitest::Test
  def test_addition
    assert_equal 4, Calculator.add(2, 2)
  end
end
```

Aja testitiedostosi suoraan tai `rake`-tehtävän kautta, joka on asetettu minitestille. Näytetuloste:
```
Ajoasetukset: --seed 33407

# Suoritetaan:

.

Valmis 0.001027s, 974.5922 ajoa/s, 974.5922 väitteitä/s.
1 ajoa, 1 väitteitä, 0 epäonnistumisia, 0 virheitä, 0 ohituksia
```

Käyttämällä näitä kirjastoja testien toteuttamiseen Ruby-projekteissasi, noudatat parhaita käytäntöjä, mikä johtaa luotettavampiin ja ylläpidettävämpiin koodikantoihin.
