---
title:                "Elixir: Kansion olemassaolon tarkistaminen"
simple_title:         "Kansion olemassaolon tarkistaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit tarkistaa, onko hakemisto olemassa? Yksi yleinen syy voisi olla halu varmistaa, että tiedostot tallennetaan oikeaan paikkaan, jotta ne voidaan myöhemmin helposti löytää.

## Miten

```Elixir
iex> File.dir?("/kansio/hakemisto")
true
```

```Elixir
iex> File.dir?("/kansio/väärä-hakemisto")
false
```

## Syväsukellus

Hakemiston tarkistaminen Elixirissä perustuu `File.dir?` -toimintoon. Tämä toiminto palauttaa `true` tai `false` riippuen siitä, onko annettu hakemisto olemassa vai ei. Tarkempi katsaus tähän toimintoon paljastaa, että se perustuu `File.stat` -toimintoon, joka käyttää `:file` -moduulia tarkistaakseen tiedoston tai hakemiston olemassaolon.

## Katso myös

- [Ilmanojennus Elixirissä](https://exercism.io/my/tracks/elixir)
- [Elixir-ohjelmointikielen viralliset kotisivut](https://elixir-lang.org/)