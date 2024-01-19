---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tulostus debug-esitys on ohjelmointitekniikka, jossa ohjelmoija tulostaa sovelluksen tilaa kuvaavia tietoja konsoliin tai tiedostoon. Sitä käytetään ohjelman virheiden tunnistamiseen ja korjaamiseen.

## Kuinka Näin:

Elixirissa debug-tietojen tulostaminen on suoraviivaista. Käytetään esimerkkinä `IO.inspect/2` funktiota:

```elixir
defmodule Hello do
  def greet do
    name = "Maailma"
    IO.inspect(name, label: "Name") # Tulostaa: Name: "Maailma"
    "Hei, #{name}"
  end
end

IO.puts Hello.greet # Tulostaa: Hei, Maailma
```
Funktion `IO.inspect/2` avulla voit tarkastella muuttujien arvoja ajon aikana. Se näyttää tiedot ja palauttaa alkuperäisen datan.

## Tarkempi Katsaus:

Historiallisessa kontekstissa, ohjelmoijat käyttivät merkkijonotulostusta virheenkorjauksessa ennen kuin debuggertyökalut ja -tekniikat olivat laajasti saatavilla. Siitä huolimatta sen käyttö on yhä merkityksellistä, erityisesti koodin käyttäytymisen nopeaan tarkasteluun.

Elixirissa on myös muita tapoja debug-tulostuksen tekemiseen. Esimerkiksi `:debugger` moduulin avulla, joka on osa Erlangin OTP-kirjastoa, voidaan ottaa käyttöön interaktiivinen debugger.

Mitä tulee toteutukseen, `IO.inspect/2` ei muuta ohjelman tilaa tai koodin suoritusjärjestystä. Se palauttaa syöttönä saamansa datan, minkä vuoksi sen voi vapaasti sijoittaa mihin tahansa koodilohkoon purkamisen vaarantamatta ohjelmalogiikkaa.

## Katso Myös:

Elixirin virallinen dokumentaatio tarjoaa runsaasti tietoa `IO.inspect/2`:sta ja muista I/O -funktioista. Tutustu myös Erlangin OTP-`debugger` moduuliin osana omia debug-taitojasi.

* [Elixir's IO.inspect](https://hexdocs.pm/elixir/IO.html#inspect/2)
* [Erlang's :debugger](http://erlang.org/doc/man/debugger.html)