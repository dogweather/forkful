---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTML:n jäsentäminen on prosessi, jossa raakamuotoinen HTML-muotoinen web-sivukoodi muunnetaan strukturoidummaksi muodoksi, jota ohjelmisto voi helpommin ymmärtää ja hallita. Ohjelmoijat tekevät tämän sisällön erittämiseksi ja käsittelemiseksi, verkko-ottimien kehittämiseksi tai datan hakemiseksi verkkosivustoilta.

## Miten?

```elixir
defmodule MyParser do
  def parse(html_code) do
    {:ok, result, _} = Floki.find(html_code, ".my-class")
    Enum.map(result, fn(elem) -> Floki.text(elem) end)
  end
end

IO.puts MyParser.parse("<div class='my-class'>Hello, World!</div>")
```

Edellä oleva koodi palauttaisi seuraavan:

```
Hello, World!
```

## Syvemmälle

HTML:n jäsentämisen historia on pitkä ja se on ollut osa nousevia kieliä ja teknologioita, kuten Python, JavaScript ja PHP. Nykyään Elixir-tuen ansiosta voimme hyödyntää sen tehokkaita rinnakkaistamistoimintoja tähän tarkoitukseen.

Erityisesti Elixirissa, `Floki` on voimakas ja suosittu vaihtoehto, joka helpottaa HTML:n jäsentämistä. On myös muita kirjastoja, kuten `Mochiweb` ja `html5ever_elixir`, mutta `Floki` on pysynyt tehokkaana työkaluna HTML:n jäsentämisessä.

Elixir-koodi toimii eri tavalla kuin monissa muissa kielissä, koska se hyödyntää rinnakkaistamista hyvän suorituskyvyn saavuttamiseksi. Se kykenee käsittelemään useita HTML-dokumentteja samanaikaisesti ilman suorituskyvyn menetystä.

## Katso Myös

- [Floki GitHub](https://github.com/philss/floki)
- [Mochiweb GitHub](https://github.com/mochi/mochiweb)
- [Parsing HTML with Elixir: A guide](https://hackernoon.com/parsing-html-with-elixir-a-how-to-guide-9eb07f7dde0c)