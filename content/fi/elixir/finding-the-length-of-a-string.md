---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Elixir: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa tarvitsemme tietää merkkijonon pituuden, kuten käsitellessämme käyttäjän syöttämää tietoa tai tarkistaessamme tiedostonimen oikeellisuuden. Elixirin avulla merkkijonon pituuden löytäminen on nopeaa ja helppoa.

## Miten

```elixir
string = "Tervetuloa Elixirin maailmaan!"

# Käytä String.length-funktiota selvittääksesi merkkijonon pituuden
String.length(string) # palauttaa 27
```

Kun käytät `String.length`-funktiota, Elixir luo uuden merkkijonon ja laskee sen merkkien määrän. Voit myös käyttää `len`-funktiota, joka löytyy `String.Chars`-moduulista.

```elixir
import String.Chars

string = "Hei maailma!"

len(string) # palauttaa 11
```

## Syvällisempi sukellus

Elixirin merkkijonofunktioissa on hyödyllisiä toimintoja merkkijonojen manipulointiin. Voit esimerkiksi käyttää `String.slice`-funktiota palauttaaksesi halutun merkkijonon osan käyttämällä indeksinumerointia.

```elixir
string = "Elixir on mahtava ohjelmointikieli!"

String.slice(string, 7..13) # palauttaa "on maht"
```

Voit myös käyttää `String.replace`-funktiota vaihtaaksesi tietyn merkkijonon toiseen.

```elixir
string = "Elixir on hieno ohjelmointikieli!"
String.replace(string, "hieno", "mahtava") # palauttaa "Elixir on mahtava ohjelmointikieli!"
```

## Katso myös

- [Elixirin virallinen dokumentaatio merkkijonofunktioista](https://hexdocs.pm/elixir/String.html)
- [Elixirin merkkijonot: yleisimpiä kysymyksiä ja vastauksia](https://elixir-lang.org/getting-started/string-patterns.html)