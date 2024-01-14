---
title:                "Elixir: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi
Testien kirjoittaminen on tärkeä osa Elixir-ohjelmointia, koska se auttaa varmistamaan koodin toimivuuden ja luotettavuuden. Testit myös helpottavat mahdollisten virheiden etsimistä ja korjaamista kehitysprosessin aikana.

## Miten
Testien kirjoittaminen Elixirissä on helppoa ja nopeaa. Alla on muutamia esimerkkejä käyttäen `assert`-moduulia:

```
# Testiluokan määrittäminen
defmodule MyTest do
  use ExUnit.Case, async: true
end

# Yksittäisen testin määrittäminen
test "lisäämisen testaaminen" do
  assert 1 + 2 == 3
end

# Testien suorittaminen komentoriviltä
$ mix test
```

Esimerkissä luodaan uusi testiluokka `MyTest`, joka sisältää yhden testin `lisäämisen testaaminen`. Käyttämällä `assert`-funktiota, voimme varmistaa, että laskutoimitus toimii oikein. Lopuksi testit suoritetaan käyttämällä `mix test` -komentoa.

Testien kirjoittaminen auttaa myös varmistamaan, että koodi toimii erilaisilla syötteillä ja reunaehtoilla. Esimerkiksi voimme testata saman laskutoimituksen eri syötteillä ja varmistaa, että lopputulos on aina oikein.

## Syvällisempi tarkastelu
Elixirin `ExUnit`-moduulin avulla voit tehdä muitakin asioita kuin vain yksinkertaisia `assert`-testejä. Esimerkiksi voit testata asynkronisia funktioita käyttämällä `async: true` -asetusta testiluokassa. Voit myös testata, että tietty virhe tulee esiin halutussa tilanteessa käyttämällä `assert_raise`-funktiota.

Testien kirjoittaminen on myös tärkeää osana jatkuvaa integrointia ja toimitamista. Automaattisten testien avulla voit varmistaa, että koodi toimii oikein myös jatkuvien muutosten ja päivitysten jälkeen.

## Katso myös
- [Elixir School](https://elixirschool.com/fi/) - Ilmainen opas Elixir-kielen oppimiseen
- [ExUnit](https://hexdocs.pm/ex_unit/ExUnit.html) - Elixirin viralliset dokumentaatiot testaamiseen
- [Elixir Testing](https://devexp.io/2020/01/28/test-driven-development-with-elixir-exunit/) - Blogipostaus testituloksesta Elixirissä