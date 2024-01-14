---
title:                "Ruby: Testien kirjoittaminen"
programming_language: "Ruby"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/writing-tests.md"
---

{{< edit_this_page >}}

# Miksi: Miksi kirjoittaisit testejä?

Kirjoittaminen testejä Ruby-ohjelmistoon saattaa tuntua ylimääräiseltä askeleelta, mutta se voi todella säästää aikaa ja vaivaa pitkällä aikavälillä. Testit varmistavat, että koodi toimii odotetulla tavalla ja auttavat havaitsemaan mahdolliset virheet ennen kuin koodi päätyy tuotantoon. Tämä takaa, että ohjelmisto toimii luotettavasti ja vähentää mahdollisia yllätyksiä jatkokehityksessä.

# Miten: Testejä kirjoittamassa

Testien kirjoittaminen Ruby-ohjelmistoon on helppoa ja yksinkertaista. Voit käyttää Ruby:n sisäänrakennettuja testejä, kuten `assert` tai `expect`, tai voit käyttää suosittuja testikehyksiä, kuten RSpec tai MiniTest.

Katsotaanpa esimerkiksi, kuinka voit kirjoittaa yksinkertaisen testin `Calculator`-luokan `add`-metodille, joka lisää yhteen kaksi lukua:

```Ruby
require "minitest/autorun"

class Calculator
  def add(num1, num2)
    num1 + num2
  end
end

class CalculatorTest < Minitest::Test
  def test_add
    calculator = Calculator.new
    assert_equal 5, calculator.add(2, 3)
  end
end
```

Ensimmäisessä koodilohkossa luomme `Calculator`-luokan, joka sisältää `add`-metodin. Sitten luomme `CalculatorTest`-luokan, joka perii `Minitest::Test`-luokan ja sisältää testin `test_add`. Tämä testi luo `Calculator`-olion ja käyttää `assert_equal`-metodia tarkistaakseen, että `add`-metodi palauttaa odotetun tuloksen.

Voit ajaa tämän testin kirjoittamalla komentoriville `ruby [tiedostonimi]`, mikä näyttää seuraavan tulosteen:

```bash
Run options: "--seed 3808"

# Running:

.

Finished in 0.001136s, 879.5967 runs/s, 879.5967 assertions/s.

1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

Tässä näet, että testi suoritettiin onnistuneesti ilman virheitä tai virheilmoituksia.

# Syvemmälle: Testien kirjoittamisessa

Testien kirjoittaminen ei rajoitu vain yksinkertaisiin metodeihin ja tulosten tarkistamiseen. Voit myös testata monimutkaisempia toimintoja ja varmistaa, että koodi toimii kaikissa mahdollisissa tapauksissa.

Esimerkiksi voit testata `Calculator`-luokan `add`-metodia useammin kuin vain yhdellä luvulla. Voit myös testata muiden operaatioiden, kuten vähennyslaskun tai kerto-laskun, toimivuuden. Näin varmistat, että kaikki laskutoimitukset toimivat oikein.

Voit myös testata, miten ohjelmisto toimii virheellisillä käyttäjän syötteillä. Tämä auttaa havaitsemaan mahdollisia virheitä ja estää ohjelmiston kaatumisen käyttäjän antaessa vääriä syötteitä.

Lisäksi voit kirjoittaa testejä, jotka suorittavat kelvollisen ja kelvottoman koodin, jotta voit selvittää, miten koodi käyttäytyy erilaisissa tilanteissa.

Testien kirjoittaminen on tärkeä osa Ruby-ohjelmiston kehitystä, joten var