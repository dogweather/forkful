---
title:    "Elixir: Virheenkorjaustulosteen tulostaminen"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi Debugger-utulostukseen kannattaa paneutua

Ilman debugger-utulostusta ohjelmointi voi olla usein arvailua ja virheiden etsiminen voi olla hankalaa ja aikaa vievää. Elixirin avulla voit helposti tulostaa debuggausviestejä, jotka auttavat sinua ymmärtämään koodia ja selvittämään mahdollisia ongelmakohtia.

## Miten tulostaa debug-tekstit Elixirissä

Aloitetaan yksinkertaisella esimerkillä, jossa luodaan funktio, joka lisää kaksi numeroa ja tulostaa virheviestin, jos lasku ei onnistu.

```Elixir
def add(x, y) do
  case x + y do
    {:error, :bad_arguments} -> IO.puts("Virhe: Väärät argumentit") # !! Tämä rivi tulostaa debug-tekstin !!
    result -> IO.puts("Tulos: #{result}")
  end
end
```

Kun kutsut funktiota `add` oikeilla argumenteilla, näet tuloksen `Tulos: <summa>`. Mutta jos käytät virheellisiä argumentteja, kuten esimerkiksi kirjaimia, saat debug-tekstin "Virhe: Väärät argumentit".

Voit myös ohjata debugger-utulostukset suoraan konsoliin käyttämällä `IO.inspect` -funktiota. Tämä tulostaa koko tietueen tai listan kaikki tiedot konsoliin, mikä voi olla hyödyllistä monimutkaisemman koodin debuggaamisessa.

```Elixir
list = [1, 2, 3, 4]
IO.inspect(list) # Tulostaa koko listan [1, 2, 3, 4]
```

## Syvempi sukellus debugger-utulostukseen

Elixirissä on myös mahdollista lisätä debugger-utulostuksia omiin moduuleihin ja funktioihin lisäämällä `@debug` -annotaation niiden alkuun. Tämä poistetaan tuotantotuotannosta, mutta tulostavat debuggaviestit auttavat kehittäjiäsi löytämään ja korjaamaan virheitä kehitysvaiheessa.

```Elixir
defmodule Calculator do
  @debug true # Tämä lisää debugger-utulostukset tähän moduuliin
  def add(x, y) do
    @debug IO.puts("Debuggausviesti: Lasketaan #{x} + #{y}")
    x + y
  end
end

result = Calculator.add(1, 2) # Tulos: 3
```

Debugger-utulostukset eivät kuluta paljon resursseja, joten voit reilusti lisätä niitä koodiisi useampiin kohtiin helpottaaksesi koodisi ymmärtämistä ja debuggaamista.

## Katso myös

- [Elixir Debugging -dokumentti](https://elixir-lang.org/getting-started/debugging.html)
- [Elixir School - Arvot ja muuttujat](https://elixirschool.com/fi/lessons/basics/values-and-variables/)
- [Elixir Tutorial - Kehitysympäristön asentaminen](https://elixir-lang.org/install.html)