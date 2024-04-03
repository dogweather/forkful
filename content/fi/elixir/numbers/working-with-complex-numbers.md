---
date: 2024-01-26 04:39:18.685492-07:00
description: "Kuinka: Elixir ei sis\xE4ll\xE4 valmiina kompleksilukuja, joten luomme\
  \ omamme tai k\xE4yt\xE4mme kirjastoa, kuten `ComplexNum`. T\xE4ss\xE4 on nopea\
  \ esimerkki kirjaston\u2026"
lastmod: '2024-03-13T22:44:56.220554-06:00'
model: gpt-4-0125-preview
summary: "Elixir ei sis\xE4ll\xE4 valmiina kompleksilukuja, joten luomme omamme tai\
  \ k\xE4yt\xE4mme kirjastoa, kuten `ComplexNum`."
title: "Kompleksilukujen k\xE4sittely"
weight: 14
---

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
