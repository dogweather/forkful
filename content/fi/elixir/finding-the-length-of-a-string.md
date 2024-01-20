---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

---
## Mikä & Miksi?

Merkkijonon pituuden määrittäminen tarkoittaa sen merkkien lukumäärän laskemista. Ohjelmoijat tekevät tämän yleensä tiedon käsittelyn ja logiikan rakentamisen helpottamiseksi.

## Näin se tehdään:

Elixirissä `String.length/1` -funktio on suunniteltu merkkijonojen pituuden mittaamiseen. Tarkastellaanpa esimerkkiä:

```elixir
IO.puts String.length("Hei maailma!")
```

Koodin suorittaminen antaa sinulle tuloksen `12`, koska "Hei maailma!" sisältää 12 merkkiä (sisältäen välilyönnin ja huutomerkin).

## Syvemmälle

- Historia: Merkkijonon pituuden määrittämisen logiikka on ollut ohjelmoinnin perusteena jo kauan. Elixir, 2011 julkaistu ohjelmointikieli, on ottanut hienosti käyttöön tämän logiikan.
  
- Vaihtoehdot: `byte_size/1` on toinen funktio, jota voidaan käyttää. Tämä ei kuitenkaan ole ideaalinen, koska se palauttaa merkkijonon bittikoon, eikä merkkien lukumäärää.
  
- Toteutus: Elixirin `String.length/1` -funktion taustalla on Binaries- ja Unicode-koodaus, mikä mahdollistaa monimutkaisten merkkijonojen pituuden mittaamisen.

## Katso myös

1. [Elixirin virallinen dokumentaatio merkkijonoista](https://hexdocs.pm/elixir/String.html#content)
2. [Erlang/Elixir Binaries, Unicode-säädetty](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html) 
3. [Elixirin koulutus: Merkkijonot](https://elixirschool.com/en/lessons/basics/strings/) 

---