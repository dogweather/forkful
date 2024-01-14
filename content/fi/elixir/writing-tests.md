---
title:    "Elixir: Testien kirjoittaminen"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi: Kirjoitamme testejä?

Kirjoittaminen testejä on tärkeä osa Elixir-ohjelmointia, sillä se auttaa varmistamaan, että koodimme toimii halutulla tavalla ja välttämään virheitä. Testien avulla voimme myös helpommin muokata ja parantaa koodia, koska tiedämme miten muutokset vaikuttavat jo olemassa olevaan koodiin.

## Kuinka: Esimerkkejä ja koodilohkoja

Testien kirjoittaminen Elixirissä on helppoa käyttämällä sisäänrakennettua ExUnit-testikehystä. Alla on esimerkki yksinkertaisesta testitapauksesta, jossa haluamme tarkistaa, että yhteenlasku toimii oikein.

```Elixir
defmodule Laskin do
  def summa(a, b) do
    a + b
  end
end

defmodule LaskinTesti do
  use ExUnit.Case
  
  test "summa palauttaa oikean tuloksen" do
    assert Laskin.summa(2, 3) == 5
  end
end
```

Yllä olevassa esimerkissä luomme moduulin nimeltä "Laskin", jossa on toiminto summa, joka laskee kahden numeron summan. Sitten testaamme tätä toimintoa luomalla LaskinTesti-moduulin ja kirjoittamalla yhden testitapauksen, jossa odotamme, että toiminto palauttaa oikean tuloksen. Käyttämällä assert-komentoa voimme tarkistaa, että toiminto palauttaa odotetun tuloksen. 

Kun ajamme testit käyttämällä "mix test" komentoa, tulisi saada viesti "1 test, 0 virhettä".

## Syvempää tietoa testeistä

Testien kirjoittaminen Elixirissä vaatii hieman tutustumista ExUnit-testikehykseen ja sen käyttöön, mutta se on ehdottomasti vaivan arvoista. Hyviä käytäntöjä testien kirjoittamisessa ovat esimerkiksi testikattavuuden tarkistaminen ja testaustölkkipakan käyttö monimutkaisempien testien kirjoittamiseen.

On myös tärkeää muistaa, että testien kirjoittaminen on jatkuva prosessi ja niitä tulisi muokata ja parantaa koodin muuttuessa.

## Katso myös

- [ExUnit-testikehyksen dokumentaatio](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Testikattavuuden tarkistaminen Elixirissä](https://elixir-lang.org/getting-started/mix-otp/docs-tests-and-with.html#code-coverage)
- [Testaustölkkipakan käyttö Elixirissä](https://github.com/lpil/elixir-test-tube)