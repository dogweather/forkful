---
title:    "Ruby: Testien kirjoittaminen"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Miksi

Testien kirjoittaminen on tärkeä osa Ruby-ohjelmointia, koska se varmistaa koodin toimivuuden ja vähentää bugin riskiä. Lisäksi testit helpottavat uusien ominaisuuksien lisäämistä ja muutosten tekemistä olemassa olevaan koodiin.

## Kuinka

Testien kirjoittaminen Rubyssa on helppoa. Voit aloittaa luomalla tiedoston, jossa on ".rb" pääte, jolloin Ruby tulkitsee sen skriptiksi. Seuraavassa esimerkissä luomme yksinkertaisen toiminnallisen testin, joka tarkistaa, että luku 2 on pienempi kuin luku 5.

```Ruby
require "minitest/autorun"

class Testi < Minitest::Test
  def testi_tarkistus
    assert(2 < 5, "2 ei ole pienempi kuin 5")
  end
end
```

Jos suoritat tämän skriptin, voit odottaa näkeväsi seuraavan tulosteen:

```
Run options: --seed 45432

# Running:

2)

Finished in 0.000672s, 2980.3551 runs/s, 2980.3551 assertions/s.

1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

Tuloste kertoo, että testi suoritettiin onnistuneesti, koska tulos oli 1, mikä tarkoittaa, että yksi testi suoritettiin ja yksi väite tehtiin. Tämä tarkoittaa, että väite "2 < 5" oli tosi.

## Syvällinen sukellus

Testit voivat olla hyvin monimutkaisia ja niillä voi olla erilaisia käyttötarkoituksia. Jotkut testit voivat tarkistaa, että koodi toimii oikein erilaisilla syötteillä, kun taas toiset voivat tarkistaa, että koodi toimii odotetulla tavalla virheellisillä syötteillä. On myös tärkeää tietää, että testien on oltava riippumattomia toisistaan ja niiden on oltava selkeitä ja ymmärrettäviä.

Testien kirjoittaminen alusta alkaen kannattaa, koska se auttaa varmistamaan, että koodi toimii oikein ja vähentää tarvetta käyttää paljon aikaa vianetsintään myöhemmin. On myös suositeltavaa kirjoittaa testit ennen varsinaisen koodin kirjoittamista, jotta voit varmistaa, että koodisi täyttää kaikki vaatimukset.

## Katso myös

- [Official Ruby Testing Guide](https://ruby-doc.org/core-3.0.0/doc/tutorial/ruby_with_selenium_and_junit.html)
- [RSpec - BDD Test Framework for Ruby](https://rspec.info/)
- [Capybara - Integration Testing Tool for Web Applications](https://github.com/teamcapybara/capybara)