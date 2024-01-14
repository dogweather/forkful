---
title:                "Elixir: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen on tärkeä osa ohjelmoinnin prosessia ja monesti joudumme muokkaamaan olemassa olevaa tekstiä. Tässä blogikirjoituksessa käsittelemme Elixiriä ja kuinka sen avulla voit helposti suorittaa tekstinhakua ja korvaamista.

## Kuinka

Elixiriä voidaan käyttää tekstin hakemiseen ja korvaamiseen kätevän `Regex`-moduulin avulla. Se sisältää monia hyödyllisiä toimintoja, kuten `replace/4`, joka mahdollistaa tekstin korvaamisen tietyllä kaavalla. Katso alla oleva esimerkki:

```Elixir
Regex.replace("Tervehdys maailma!", ~r/maailma/, "Suomi")

Output: Tervehdys Suomi!
```

Voit myös käyttää `Regex.run/3`-funktiota, joka palauttaa osumien listan, joka voidaan sitten käsitellä halutulla tavalla. Katso alla oleva esimerkki:

```Elixir
Regex.run("Jaana, Johanna, ja Jussi ovat ystäviä.", ~r/[Jj]a+na/, capture: :all_but_first)

Output: [Johanna, Jussi]
```

## Syvemmälle

Elixirin `Regex`-moduuli tarjoaa myös muita hyödyllisiä ominaisuuksia, kuten `match?`-funktion, joka palauttaa totuusarvon riippuen siitä, onko tekstissä yhtään osumaa hakuehdolle. Voit myös käyttää `replace/3`-funktiota, joka korvaa kaikki osumat halutulla merkkijonolla.

Jos haluat tutustua Elixirin `Regex`-moduulin tarjontaan tarkemmin, voit lukea siitä lisää [Elixirin virallisesta dokumentaatiosta](https://hexdocs.pm/elixir/Regex.html).

## Katso myös

- [Regexin käyttö Elixirissä](https://blog.red-badger.com/blog/2016/4/18/get-in-awesomely-good-elixir)
- [10 asiaa, joita tulisi tietää Elixiristä](https://hackernoon.com/10-things-to-know-about-elixir-2428a9c1da3a)
- [Elixirin viralliset verkkosivut](https://elixir-lang.org/)