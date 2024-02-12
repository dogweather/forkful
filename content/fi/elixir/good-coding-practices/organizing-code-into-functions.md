---
title:                "Koodin järjestäminen funktioihin"
aliases:
- /fi/elixir/organizing-code-into-functions/
date:                  2024-01-26T01:09:53.033984-07:00
model:                 gpt-4-1106-preview
simple_title:         "Koodin järjestäminen funktioihin"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Koodin järjestäminen funktioiksi tarkoittaa, että liittyvät operaatiot ryhmitellään uudelleenkäytettäviksi lohkoiksi. Tämä tehdään luettavuuden ja ylläpidettävyyden parantamiseksi, toiston vähentämiseksi ja testauksen yksinkertaistamiseksi.

## Miten:
Luodaan yksinkertainen Elixir-funktio sanojen alkukirjainten suurentamiseen:

```elixir
defmodule StringUtils do
  def capitalize_words(sentence) do
    sentence
    |> String.split()
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end
end

IO.puts StringUtils.capitalize_words("hello elixir world")
```
Tuloste:
```
Hello Elixir World
```
Tässä olemme tyylikkäästi paketoineet sanankapitalisointilogiikan nimeltä `capitalize_words` funktioon.

## Syväsukellus
Elixirissä, ja laajemmassa Erlang VM -ekosysteemissä, funktiot ovat ensiluokkaisia kansalaisia, perien filosofian ongelmien pilkkomisesta pienemmiksi, hallittaviksi ja eristetyiksi osiksi. Historiallisesti tämä funktionaalinen lähestymistapa juontaa juurensa lambda kalkyylistä ja Lispeistä, edistäen koodin kuin datan filosofiaa.

Vaihtoehtoja koodin järjestämiselle voivat olla makrojen tai prosessien käyttäminen Elixirissä toistuvia tai samanaikaisia tehtäviä varten, vastaavasti. Toteutuksen kannalta Elixirin funktiot voivat käsitellä kuviorivastusta ja vastaanottaa erilaisia argumentteja (arity), mikä antaa niille monipuolisuutta.

## Katso myös
- [Elixirin virallinen dokumentaatio funktioista](https://hexdocs.pm/elixir/Kernel.html#functions)
- [Dave Thomasin "Programming Elixir"](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
