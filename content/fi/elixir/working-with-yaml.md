---
title:                "Työskentely yaml:n kanssa"
html_title:           "Elixir: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

Uusi vuosi, uudet Elixir-taidot – tämä artikkeli auttaa sinut alkuun YAML:n kanssa. Tämä ahkeraan käytetty tiedostomuoto on suosittu ohjelmoijilla, ja tutustumalla siihen voit monipuolistaa ohjelmointikokemustasi.

## Mitä & Miksi?

YAML on tapa tallentaa dataa luettavassa muodossa. Se auttaa ohjelmoijia jakamaan ja tallentamaan tietoja helposti. YAML on myös helpompi lukea ja ymmärtää kuin perinteinen JSON-tiedosto.

## Kuinka:

Elixirissä YAML:n käyttö on helppoa ja yksinkertaista. Voit ladata YAML-paketin koodiisi käyttämällä ```mix.exs```-tiedostoa seuraavasti:

```
defp deps do
  [{:yaml, "~> 0.2.0"}]
end
```

Tämän jälkeen voit käyttää YAML-pakettia koodissasi seuraavasti:

```
YAML.decode("""
- name: John
  age: 27
- name: Sarah
  age: 33
""")
```

Tämä koodi palauttaa listan karttoja, joissa on "name" ja "age" -avaimet. Tuloste näyttää tältä:

```
[%{"age" => 27, "name" => "John"}, %{"age" => 33, "name" => "Sarah"}]
```

Käyttämällä YAML.encode-funktiota voit muuttaa tietorakenteen YAML-muotoon:

```
YAML.encode(%{name: "Elixir", version: "1.10"})
```

Tämä koodi palauttaa seuraavan YAML-muotoisen tulosteen:

```
"name": "Elixir"
"version": "1.10"
```

## Syväsukellus:

YAML kehitettiin ensimmäisen kerran vuonna 2001, ja sen tarkoituksena oli korvata monimutkaisten XSLT- tiedostojen käyttö XML:n kanssa. Nykyään YAML on yleisempi kuin XML monissa sovelluksissa, koska sen avulla tiedostojen lukeminen ja kirjoittaminen on nopeampaa ja helpompaa.

On myös muita vaihtoehtoja YAML:lle, kuten CSV- ja INI-tiedostomuodot. CSV on hyvä vaihtoehto jos haluat tallentaa yksinkertaisia listoja, mutta jos haluat tallentaa monimutkaisempia dataa, or INI-tiedostot eivät ole paras valinta.

YAML-paketti Elixirissä perustuu libyaml-kirjastoon, ja se käyttää C-koodia parantaakseen suorituskykyä. Tästä syystä YAML on yksi nopeimmista tiedostomuodoista Elixirissä.

## Lue lisää:

Jos haluat oppia lisää YAML:stä, voit tutustua paketin viralliseen dokumentaatioon: https://hexdocs.pm/yaml/readme.html. Voit myös tarkastella libyaml-kirjaston dokumentaatiota täällä: http://pyyaml.org/wiki/LibYAML.

Onnea matkaan YAML:n kanssa – toivon, että tästä artikkelista on sinulle hyötyä Elixir-taitojesi kehittämisessäsi!