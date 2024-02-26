---
date: 2024-01-26 00:51:49.300220-07:00
description: "Virheiden k\xE4sittely tarkoittaa koodin kirjoittamista, joka pystyy\
  \ k\xE4sittelem\xE4\xE4n kun asiat menev\xE4t pieleen. Ohjelmoijat tekev\xE4t sit\xE4\
  \ est\xE4\xE4kseen kaatumiset\u2026"
lastmod: '2024-02-25T18:49:53.207631-07:00'
model: gpt-4-1106-preview
summary: "Virheiden k\xE4sittely tarkoittaa koodin kirjoittamista, joka pystyy k\xE4\
  sittelem\xE4\xE4n kun asiat menev\xE4t pieleen. Ohjelmoijat tekev\xE4t sit\xE4 est\xE4\
  \xE4kseen kaatumiset\u2026"
title: "Virheiden k\xE4sittely"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Virheiden käsittely tarkoittaa koodin kirjoittamista, joka pystyy käsittelemään kun asiat menevät pieleen. Ohjelmoijat tekevät sitä estääkseen kaatumiset ja varmistaakseen, että ohjelmat voivat toipua arvokkaasti, kun Murphy’n laki iskee.

## Kuinka:

Elixirissä käytämme usein mallin vastaavuutta ja `case`-lauseketta eri lopputulosten, mukaan lukien virheet, käsittelyyn.

```elixir
defmodule Example do
  def divide(a, b) do
    case b do
      0 -> {:error, "Nollalla jakaminen ei ole mahdollista."}
      _ -> {:ok, a / b}
    end
  end
end

# Onnistunut jako
{:ok, result} = Example.divide(10, 2)
IO.puts("10 / 2 on #{result}")

# Yritys jakaa nollalla
{:error, reason} = Example.divide(10, 0)
IO.puts("Virhe: #{reason}")
```

Esimerkkitulo:
```
10 / 2 on 5.0
Virhe: Nollalla jakaminen ei ole mahdollista.
```

Kun ajat tämän Elixir-koodin, saat joko onnistuneen jakotuloksen tai virheilmoituksen riippuen syötteestäsi. Ei kaatumisia täällä!

## Syväsukellus

Aikaisemmin virheiden käsittely oli usein paluuarvojen tarkistamista. Elixirin funktionaalisten juurien ansiosta meillä on nyt mallin vastaavuus ja merkityt tuplat, kuten `{:ok, value}` tai `{:error, reason}`, jotka ovat elegantimpi ratkaisu.

Elixirissä on muitakin tapoja käsitellä virheitä:

- **Elixirin `try` ja `rescue`**, jotka muistuttavat imperatiivisten kielien perinteistä `try-catch` -rakennetta, mutta joita käytetään harvemmin Elixirin eksplisiittisyyden suosimisen vuoksi.
- **Supervisorit ja GenServerit**, osa Elixirin OTP-kehyksestä, jotka keskittyvät enemmän vikasietoisuuteen. Ne tarkkailevat koodisi prosessia, valmiina käynnistämään sen uudelleen, jos jotain menee vikaan.

Toteutuksen kannalta Elixir rakentuu Erlangin luotettavuuden varaan. Se käsittelee virheitä vain toisen tyyppisenä viestinä, joka käsitellään kaiken mallin vastaavuuden ja funktionaalisen hyvyyden avulla.

## Katso Myös

Lisätietoa Elixirin virheenkäsittelystä, tutustu:

- Elixirin virallinen opas [virheenkäsittelystä](https://elixir-lang.org/getting-started/try-catch-and-rescue.html).
- Lisätietoa [prosesseista ja OTP:stä](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html).
- Elixir Forum on aina hyvä paikka kysyä kysymyksiä: [https://elixirforum.com](https://elixirforum.com).
