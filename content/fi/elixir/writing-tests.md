---
title:    "Elixir: Testien kirjoittaminen"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Miksi

Testien kirjoittaminen on tärkeä osa Elixir-ohjelmoinnin prosessia, sillä se auttaa varmistamaan koodin laadun ja toimivuuden. Testien avulla voit myös varmistaa, että muutokset eivät aiheuta regressioita koodiisi.

## Kuinka kirjoittaa testeja Elixirissä

Testien kirjoittaminen Elixirissä on helppoa ja suoraviivaista. Voit aloittaa määrittelemällä testin tiedoston yläreunassa olevan moduulin nimen ja käyttämällä `ExUnit.Case` -moduulia. Testit voivat olla joko funktioita tai makroja, mutta makrojen käyttö on suositeltavaa, sillä niiden avulla voit hyödyntää Elixirin hahmontunnistusta.

```Elixir
defmodule Testi do
  use ExUnit.Case
  
  test "summaa kaksi lukua" do
    summa = 2 + 2
    assert summa == 4
  end
end
```

Testin voit suorittaa ajamalla `mix test` -komennon terminaalissa. Jos kaikki testit suoritettiin onnistuneesti, näet viestin `All 1 tests passed` ja jos jokin testi epäonnistuu, näet tarkemman virheviestin ja rivinumeron, josta virhe löytyi.

## Syvällisempi opas testien kirjoittamiseen

Testien kirjoittaminen Elixirissä perustuu erilaisiin asserointeihin. Yleisimpiä asserointeja ovat `assert` ja `refute`, jotka tarkistavat, että annettu väite on tosi tai epätosi. Voit myös käyttää muita asserointikomentoja, kuten `assert_equal`, `refute_in_delta` ja `assert_raise`.

Lisäksi voit määrittää ennen ja jälkeen jokaisen testin suorittamisen erityisesti määritellyt funktiot käyttämällä `setup` ja `teardown` -makroja. Näiden avulla voit esimerkiksi alustaa tarvittavat muuttujat ennen testin suorittamista tai siivota testin jälkeen.

Kun kirjoitat testejä, on myös tärkeää ottaa huomioon testien järjestys. Testit suoritetaan aina aakkosjärjestyksessä, joten muutokset koodissa voivat vaikuttaa muiden testien suoritukseen. Siksi on tärkeää kirjoittaa testit siten, että ne ovat riippumattomia toisistaan.

## Katso myös

- ExUnit - Elixirin testikehys: https://hexdocs.pm/ex_unit/
- Testien kirjoittaminen Elixirissä: https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html#unit-tests
- Elixirin hahmontunnistus: https://elixir-lang.org/getting-started/pattern-matching.html