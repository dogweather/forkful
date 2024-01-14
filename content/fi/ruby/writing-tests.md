---
title:                "Ruby: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kirjoittaa testeja? Testien kirjoittaminen on tärkeä osa hyvää ohjelmointia, sillä se varmistaa koodin laadun ja vähentää virheiden mahdollisuutta. Testaus myös helpottaa koodin ylläpitämistä ja mahdollistaa uusien ominaisuuksien lisäämisen ilman, että se rikkoo jo olemassa olevaa koodia.

## Kuinka tehdä

Testien kirjoittaminen Rubyssa on helppoa ja intuitiivista. Käytämme tähän Ruby-on-Railsin sisäänrakennettua testauskehystä, nimeltään MiniTest.

### Asennus

Asentaaksesi MiniTestin, voit käyttää RubyGems-pakettienhallintajärjestelmää. Avaa terminaali ja suorita seuraavat komennot:

```bash
gem install minitest
```

### Esimerkki

Tarkastellaan esimerkkinä ohjelmaa, joka tarkistaa annetun luvun parillisuuden. 

```ruby
# Ohjelma tarkistaa, onko luku parillinen vai ei
def onko_parillinen?(luku)
  luku % 2 == 0 
end

# Testien kirjoittaminen
require 'minitest/autorun'

# Testiluokka
class ParillinenTesti < Minitest::Test
  # Testi, joka varmistaa, että luku 2 on parillinen
  def test_parillinen_luku
    tulos = onko_parillinen?(2)
    assert_equal(true, tulos)
  end
  
  # Testi, joka varmistaa, että luku 3 ei ole parillinen
  def test_pariton_luku
    tulos = onko_parillinen?(3)
    assert_equal(false, tulos)
  end
end
```

### Tulostus

Suorita testit komennolla `ruby nimi_testikoodi.rb` ja tulostuksen tulisi olla seuraavanlainen:

```bash
Run options: --seed 62838

# Running:

..

Finished in 0.000615s, 3248.8101 runs/s, 4873.2151 assertions/s.

2 runs, 3 assertions, 0 failures, 0 errors, 0 skips
```

## Syvällinen tarkastelu

Testien kirjoittaminen ei ole vain yksinkertainen prosessi, vaan sitä on hyvä syvällisemmin tutkia ja ymmärtää. Testit eivät vain varmista koodin toimintaa, vaan myös auttavat ymmärtämään omaa koodia paremmin. Ne pakottavat meidät miettimään erilaisia skenaarioita ja testaamaan koodia eri tavoin. Tämä johtaa parempaan suunnitteluun ja laadukkaampaan koodiin.

## Katso myös

Jos haluat oppia lisää testien kirjoittamisesta Rubyssa, suosittelemme seuraavia resursseja:

- [Minitest Documentation] (https://github.com/seattlerb/minitest#mini-testrdoc)
- [Ruby Testing Tools] (https://blog.honeybadger.io/best-ruby-testing-tools/)
- [Test Driven Development with Ruby] (https://www.codementor.io/ruby-on-rails/tutorial/test-driven-development-ruby-test-unit)

Kiitos lukemisesta! Toivottavasti tämä artikkeli auttoi sinua ymmärtämään testien kirjoittamisen tärkeyden ja antoi sinulle hyödyllisiä vinkkejä Ruby-testaamiseen. Onnea testaamiseen ja hyvän koodin kirjoittamiseen!