---
title:                "Testien kirjoittaminen"
aliases:
- /fi/elixir/writing-tests.md
date:                  2024-02-03T19:30:26.697342-07:00
model:                 gpt-4-0125-preview
simple_title:         "Testien kirjoittaminen"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mitä & Miksi?
Testien kirjoittaminen Elixirissä sisältää automatisoitujen skriptien luomisen koodisi toiminnan varmentamiseksi. Ohjelmoijat tekevät tämän varmistaakseen laadun, estääkseen regressiot ja helpottaakseen koodin uudelleenjärjestämistä, mikä tekee kehitysprosessista luotettavamman ja tehokkaamman.

## Kuinka:
Elixir käyttää sisäänrakennettuna testikehyksenään ExUnitia, joka on erittäin tehokas ja helppokäyttöinen. Tässä on yksinkertainen esimerkki:

1. Luo uusi testitiedosto `test`-hakemistoon Elixir-projektissasi. Esimerkiksi, jos testaat moduulia nimeltä `MathOperations`, testitiedostosi voisi olla `test/math_operations_test.exs`.

```elixir
# test/math_operations_test.exs
defmodule MathOperationsTest do
  use ExUnit.Case

  # Tämä on yksinkertainen testitapaus tarkistamaan yhteenlaskufunktion
  test "kahden numeron yhteenlasku" do
    assert MathOperations.add(1, 2) == 3
  end
end
```

Suorittaaksesi testisi, käytä `mix test`-komentoa terminaalissasi. Jos `MathOperations.add/2`-funktio laskee kaksi numeroa oikein yhteen, näet tulosteen, joka on samankaltainen kuin:

```
..

Valmis 0.03 sekunnissa
1 testi, 0 epäonnistumista
```

Testeissä, jotka liittyvät ulkoisiin palveluihin tai API:hin, saatat haluta käyttää mock-kirjastoja, kuten `mox`, välttääksesi oikeiden palveluiden käytön:

1. Lisää `mox` riippuvuuksiisi `mix.exs`-tiedostoon:

```elixir
defp deps do
  [
    {:mox, "~> 1.0.0", only: :test},
    # muita riippuvuuksia...
  ]
end
```

2. Määritä mock-moduuli testiapurissasi (`test/test_helper.exs`):

```elixir
Mox.defmock(HTTPClientMock, for: HTTPClientBehaviour)
```

3. Käytä mockia testitapauksessasi:

```elixir
# test/some_api_client_test.exs
defmodule SomeAPIClientTest do
  use ExUnit.Case
  import Mox

  # Tämä käskee Moxia varmistamaan, että tämä mock kutsuttiin odotetulla tavalla
  setup :verify_on_exit!

  test "hakee dataa API:sta" do
    # Aseta mock-vastaus
    expect(HTTPClientMock, :get, fn _url -> {:ok, "Mock-vastaus"} end)
    
    assert SomeAPIClient.get_data() == "Mock-vastaus"
  end
end
```

Kun suoritat `mix test`, tämä asetus mahdollistaa yksikkötestiesi eristämisen todellisista ulkoisista riippuvuuksista, keskittyen oman koodisi käyttäytymiseen. Tämä malli varmistaa, että testisi suoritetaan nopeasti ja pysyvät luotettavina riippumatta ulkoisen palvelun tilasta tai internet-yhteydestä.
