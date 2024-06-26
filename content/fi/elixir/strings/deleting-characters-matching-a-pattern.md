---
date: 2024-01-20 17:41:57.682674-07:00
description: "How to: \"N\xE4in teet:\" Elixiriss\xE4 RegExp-moduuli (regex) on ty\xF6\
  kalusi kun haluat poistaa merkkej\xE4. T\xE4ss\xE4 on esimerkki koodi miten poistat\
  \ kaikki numerot\u2026"
lastmod: '2024-04-05T21:53:57.767330-06:00'
model: gpt-4-1106-preview
summary: "\"N\xE4in teet:\" Elixiriss\xE4 RegExp-moduuli (regex) on ty\xF6kalusi kun\
  \ haluat poistaa merkkej\xE4."
title: Merkkien poistaminen hakemalla osumia kaavaan
weight: 5
---

## How to:
"Näin teet:"
Elixirissä RegExp-moduuli (regex) on työkalusi kun haluat poistaa merkkejä. Tässä on esimerkki koodi miten poistat kaikki numerot merkkijonosta:

```elixir
string = "Elixir 1 on 2 mahtava 3 ohjelmointikieli!"
pattern = ~r/[0-9]/
clean_string = String.replace(string, pattern, "")
IO.puts(clean_string)
```

Tulos:
```
Elixir  on  mahtava  ohjelmointikieli!
```

Ja jos haluat poistaa välilyönnit:

```elixir
string = "Elixir on    mahtava ohjelmointikieli!"
clean_string = String.replace(string, ~r/[\s]+/, " ", global: true)
IO.puts(clean_string)
```

Tulos:
```
Elixir on mahtava ohjelmointikieli!
```

## Deep Dive
"Sukellus syvyyksiin"
Elixirin String-moduulin `replace/4`-functio on modernin Erlang-ekosysteemin kätevä perintö. Historiallisesti, monien kielten string-käsittelyn juuret ovat C:n libraryissä, mutta Elixir, kuten Erlang, hyötyy vahvasti toiminnallisen ohjelmoinnin paradigmoista, tekee koodistasi tiivistä ja helppolukuista. Vaihtoehtoiset menetelmät merkkien poistamiseen sisältävät `String.replace/3` ilman regex patternia ja `Regex.replace/3`, joka on matalamman tason kirjasto. Performance riippuu käytettävästä metodista, regex patternin monimutkaisuudesta, ja merkkijonon pituudesta. Silmälläpitäen, että Elixir käsittelee binäärejä tehokkaasti, edes pitkät merkkijonot eivät yleensä tuota ongelmia.

## See Also
"Katso myös"
- Elixirin viralliset dokumentit `String`-moduulista: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html) 
- Regex-moduulin viralliset dokumentit: [https://hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)
- Online regex-testaustyökalu (hieno tapa kokeilla regex-patternit ennen koodiin lisäämistä): [https://regex101.com/](https://regex101.com/)
