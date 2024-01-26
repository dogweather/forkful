---
title:                "Merkkijonon pituuden selvittäminen"
date:                  2024-01-20T17:47:28.121243-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
"Mikä & Miksi?"
Stringin pituuden selvittäminen tarkoittaa merkkijonossa olevien merkkien määrän laskemista. Ohjelmoijat tekevät tämän, jotta voivat hallita ja validoida syötteitä tai käsitellä tekstin pituudesta riippuvia toimintoja.

## How to:
"Näin teet:"
Elixirisssa stringin pituuden saa selville `String.length/1` funktiolla. Helppoa ja yksinkertaista.

```elixir
string = "Hei maailma!"
pituus = String.length(string)
IO.puts(pituus)
```

Tämän pitäisi tulostaa:

```
12
```

Toinen esimerkki, jossa on erikoismerkkejä:

```elixir
string = "Hänellä on 10€"
pituus = String.length(string)
IO.puts(pituus)
```

Tulostaa:

```
14
```

Huomaa, että special merkit lasketaan yksittäisinä merkkeinä.

## Deep Dive
"Sukellus syvemmälle"
Stringien pituuden laskeminen on perushommaa ja osa ohjelmoinnin perusteita – oli kyseessä mikä kieli tahansa. Historiallisesti, monet kielet käyttivät nollalla päättyviä merkkijonoja, mikä asetti omat rajoituksensa. Elixirissä, kuten muissakin moderneissa kielissä, stringit ovat UTF-8-koodattuja, joten `String.length/1` ottaa huomioon Unicode-merkit oikein.

Vaihtoehtona, voit käyttää `byte_size/1`, jos tarvitset tietää montako tavua string vie muistissa, mutta se ei yleensä ole sama kuin merkkien määrä:

```elixir
string = "Älä huuda!"
IO.puts(byte_size(string))    # Tulostaa tavujen määrän
IO.puts(String.length(string))  # Tulostaa merkkien määrän
```

Tulostaa:

```
11
9
```

## See Also
"Katso myös"
- Elixirin virallinen [String moduuli dokumentaatio](https://hexdocs.pm/elixir/String.html)
- [Unicode standardi](http://www.unicode.org/standard/standard.html)
- [Elixir School](https://elixirschool.com/en/): Oppitunteja Elixirin perusteista, myös merkkijonoista.
