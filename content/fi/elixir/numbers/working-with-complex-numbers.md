---
title:                "Kompleksilukujen käsittely"
date:                  2024-01-26T04:39:18.685492-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kompleksilukujen käsittely"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Mikä ja Miksi?
Kompleksiluvuilla on reaaliosa ja imaginääriosa (kuten `3 + 4i`). Niitä käytetään tekniikassa, fysiikassa sekä tietyissä tietojenkäsittelyn ongelmissa. Ohjelmoijat työskentelevät niiden kanssa simulaatioissa, signaalinkäsittelyssä ja tietyntyyppisten matemaattisten ongelmien tehokkaassa ratkaisemisessa.

## Kuinka:
Elixir ei sisällä valmiina kompleksilukuja, joten luomme omamme tai käytämme kirjastoa, kuten `ComplexNum`. Tässä on nopea esimerkki kirjaston kanssa:

```elixir
# Olettaen, että sinulla on ComplexNum asennettuna
defmodule ComplexMath do
  import ComplexNum

  def add(a, b) do
    ComplexNum.add(a, b)
  end
end

# Luodaan kompleksiluvut ja lisätään ne yhteen
c1 = {3, 4}   # edustaa 3 + 4i
c2 = {2, -3}  # edustaa 2 - 3i
tulos = ComplexMath.add(c1, c2)
IO.puts "Tulos on: #{inspect(tulos)}"
```

Tämä tulostaisi:
```
Tulos on: {5, 1}
```

Se tarkoittaa, että `3 + 4i` ja `2 - 3i` summa on `5 + 1i`.

## Syväsukellus
Kompleksiluvut tulivat historiaan, koska tavalliset vanhat luvut eivät pystyneet käsittelemään negatiivisten lukujen neliöjuuria. Vasta 17. vuosisadalla niitä alettiin ottaa vakavasti kiitos matemaatikkojen, kuten René Descartesin ja Gerolamo Cardanon.

Elixirissä käytetään usein tupleja, kuten `{3, 4}`, kompleksilukujen esittämiseen, tai käytetään omistautunutta kirjastoa, jotta ei tarvitse keksiä pyörää uudestaan. Kirjastot ovat yleensä parempia – ne käsittelevät hankalia yksityiskohtia, kuten kertolaskua ja jakolaskua, jotka muuttuvat hankaliksi imaginääriyksikön 'i' takia (FYI: `i` toiseen on `-1`).

## Katso Myös
Tutustu näihin resursseihin:
- [ComplexNum-kirjasto](https://hex.pm/packages/complex_num) Elixiriin Hex-paketinhallinnassa.
- [Elixir School](https://elixirschool.com/en/), edistyneisiin Elixir-aiheisiin ja harjoituksiin.
- [Erlang -- math-moduuli](http://erlang.org/doc/man/math.html), jota Elixir käyttää taustalla, muihin matemaattisiin tarpeisiin.
