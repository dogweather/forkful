---
title:                "Html-parsiminen"
html_title:           "Elixir: Html-parsiminen"
simple_title:         "Html-parsiminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
HTML-parsiminen tarkoittaa web-sivun rakenteen analysointia ja siitä tietojen keräämistä. Ohjelmoijat tekevät sitä usein web-scrapingin yhteydessä, jotta he voivat automatisoida tiedon keräämistä ja käyttöä.

## Kuinka:
```Elixir
# Esimerkki HTML-sivun lataamisesta ja parsimisesta käyttäen Floki-kirjastoa
html = Floki.parse(html_sivu)
# Otsikoiden kerääminen
otsikot = html
          |> Floki.find("h1")
          |> Enum.map(&(&1.children))
          |> Enum.map(&(&1[0]))
```

Tulostus:
```
["Ensimmäinen otsikko"]
```

## Syväluotaus:
HTML-parsiminen alkoi jo 1990-luvulla, kun internet oli vasta kehittymässä ja koneet tarvitsivat tapoja kommunikoida keskenään. Nykyään on muitakin tapoja hakea tietoja websivuilta, kuten ScrapingHub ja Beautiful Soup. Elixirin Floki-kirjasto hyödyntää HXT-kirjastoa ja mahdollistaa HTML-rakenteen käsittelyn Elixirillä.

## Katso myös:
- https://scrapinghub.com/what-is-web-scraping/
- https://ksxgithub.github.io/blog/2018/06/30/of-beautifulsoup4-scrapy-floki-hexlib-hxhtml-hxt-scrapinghub-'
- https://github.com/elixirabs/hxt