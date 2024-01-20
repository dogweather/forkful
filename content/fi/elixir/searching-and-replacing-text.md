---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Tekstin etsiminen ja korvaaminen on prosessi, jossa ohjelma tunnistaa jonkin tekstin ja korvaa sen toisella. Ohjelmoijat tekevät tätä monimutkaisten merkkijonojen käsittelyn, datan uudelleenmuotoilun tai virheiden korjaamisen yhteydessä.

## Näin se tehdään:

Käytännössä voimme käyttää Elixirin `String.replace/3` -funktiota tähän tarkoitukseen. Katsotaan esimerkkikoodi:

```elixir
defmodule TextReplacer do
  def replace(input, find, replace) do
    String.replace(input, find, replace)
  end
end

IO.puts TextReplacer.replace("Tervetuloa Elixir-maailmaan!", "maailmaan", "ohjelmointiin")
```

Tämä ohjelma korvaa sanan "maailmaan" sanalla "ohjelmointiin", joten ohjelman tulostus olisi "Tervetuloa Elixir-ohjelmointiin!".

## Deep Dive

Tekstin etsimisen ja korvaamisen käsite ei ole uusi ja se löytyy lähes kaikista ohjelmointikielistä. Elixiriä kehitettiin pitkälti suorituskykyisen ja tehokkaan rinnakkais- ja samanaikaisohjelmoinnin mahdolliseksi tekemiseen. Siksi sen merkkijonokäsittely on suunniteltu ottamaan huomioon monimutkaisemmat asiayhteydet.

Vaihtoehtoisesti voit käyttää suoraa säännöllistä ilmausta `Regex.replace/3` -funktiolla, jos etsit jotakin monimutkaisempaa.

`String.replace/3` ja `Regex.replace/3` eroavat toisistaan siten, että `String.replace/3` on yksinkertaisille muutoksille, kun taas `Regex.replace/3` tarjoaa joustavuutta säännöllisten lausekkeiden avulla.

## Katso myös:

Elixirin merkkijonokäsittelyyn liittyviä resursseja ja ohjeita voit löytää seuraavien linkkien kautta:

- Elixirin String-moduuli: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- Elixirin Regex-moduuli: [https://hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)
- Elixirin merkkijonojen ja säännöllisten lausekkeiden opas: [https://elixircasts.io/string-replacement-and-regex](https://elixircasts.io/string-replacement-and-regex)
 
Muista, että paras tapa oppia on käytäntö, joten sovella tietojasi ja koodaa!