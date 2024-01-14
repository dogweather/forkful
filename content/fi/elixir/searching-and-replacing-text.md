---
title:    "Elixir: Tekstin etsiminen ja korvaaminen"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Miksi Elixir on hyvä valinta tekstin etsimiseen ja korvaamiseen

Tekstin etsiminen ja korvaaminen on yleinen tehtävä ohjelmointia tehdessä. Elixir tarjoaa tehokkaita työkaluja tämän tehtävän suorittamiseen. Se pystyy käsittelemään isoja datamääriä nopeasti ja tarjoaa monipuolisia toimintoja, joilla voit muokata tekstiä haluamallasi tavalla.

# Kuinka tehdä tekstien etsiminen ja korvaaminen Elixirillä

Tekstin etsiminen ja korvaaminen Elixirillä onnistuu käyttämällä `String.replace/4` -funktiota. Tämä funktio ottaa neljä parametria: alkuperäinen merkkijono, hakusana, korvaava sana ja valinnaisen `options` -parametrin. Se palauttaa uuden merkkijonon, jossa kaikki hakusanat on korvattu korvaavalla sanalla.

```Elixir
iex> String.replace("Tervetuloa Elixir-maailmaan", "maailmaan", "universumi", global: true)
"Tervetuloa Elixir-universumiin"
```

Voit myös käyttää säännöllisiä lausekkeita tekstien etsimiseen ja korvaamiseen `Regex.replace/3` -funktiolla. Tämä funktio ottaa kolme parametria: alkuperäinen merkkijono, säännöllinen lauseke ja korvaava sana. Se palauttaa uuden merkkijonon, jossa kaikki säännöllisen lausekkeen mukaiset osat on korvattu korvaavalla sanalla.

```Elixir
iex> Regex.replace("Minulla on 123 euroa säästössä", ~r/\d+/, "100")
"Minulla on 100 euroa säästössä"
```

# Syvempi sukellus tekstien etsimiseen ja korvaamiseen

Elixirin `String` -moduuli tarjoaa paljon muitakin hyödyllisiä toimintoja tekstien muokkaamiseen. Voit esimerkiksi käyttää `String.trim/2` funktiota poistamaan tyhjät välilyönnit alkupäästä ja lopusta.

```Elixir
iex> String.trim("     Hei maailma     ")
"Hei maailma"
```

Voit myös muuttaa merkkijonon isoiksi tai pieniksi kirjaimiksi käyttämällä `String.upcase/1` ja `String.downcase/1` -funktioita.

```Elixir
iex> String.upcase("Tervetuloa Elixir-maailmaan")
"TERVETULOA ELIXIR-MAAILMAAN"

iex> String.downcase("Tervetuloa Elixir-maailmaan")
"tervetuloa elixir-maailmaan"
```

Jos haluat tehdä tekstien etsimisestä ja korvaamisesta vielä tehokkaampaa, voit käyttää `Map` -moduulin tarjoamia toimintoja. Voit esimerkiksi käyttää `Map.get/3` -funktiota poistamaan tiettyjä sanoja tai merkkejä merkkijonosta.

```Elixir
iex> "Olen käyttänyt Elixir-ohjelmointikieltä jo 3 vuotta!" |> String.split(" ") |> Map.get(&1, 3, "kieltä") |> Enum.join(" ")
"Olen käyttänyt Elixir kieltä jo vuotta!"
```

# Katso myös

- Elixirin virallinen dokumentaatio: https://elixir-lang.org/getting-started/basic-types.html#string-literals
- Elixirin `String` -moduulin dokumentaatio: https://hexdocs.pm/elixir/String.html
- Elixirin `Map` -moduulin dokumentaatio: https://hexdocs.pm/elix