---
date: 2024-01-20 17:47:28.121243-07:00
description: "How to: \"N\xE4in teet:\" Elixirisssa stringin pituuden saa selville\
  \ `String.length/1` funktiolla. Helppoa ja yksinkertaista."
lastmod: '2024-03-13T22:44:56.217413-06:00'
model: gpt-4-1106-preview
summary: "\"N\xE4in teet:\"\nElixirisssa stringin pituuden saa selville `String.length/1`\
  \ funktiolla."
title: "Merkkijonon pituuden selvitt\xE4minen"
weight: 7
---

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
