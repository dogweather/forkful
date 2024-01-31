---
title:                "HTML:n jäsentäminen"
date:                  2024-01-20T15:31:05.744077-07:00
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"

category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTML:n jäsentäminen tarkoittaa HTML-koodin rakenteen muuttamista datarakenteiksi, joita ohjelmat voivat käsitellä. Ohjelmoijat tekevät tätä esimerkiksi verkkosivustojen tietojen kaapaukseen tai sisällön manipulointiin.

## Kuinka:

Elixirissä HTML:n jäsentämistä varten voidaan käyttää esimerkiksi `Floki`-kirjastoa, joka tukee CSS-selektoreita datan kaapauksessa.

```elixir
# Lisää `Floki` riippuvuuksiin mix.exs-tiedostossa
defp deps do
  [
    {:floki, "~> 0.30.0"}
  ]
end

# Jäsentä esimerkki HTML-dokumentti
def parse_html(html) do
  {:ok, parsed_html} = Floki.parse(html)
  titles = Floki.find(parsed_html, "h1")
  Floki.text(titles)
end

# Käyttö esimerkki
html = "<html><body><h1>Hei Suomi!</h1></body></html>"
IO.inspect(parse_html(html)) # Tulostaa "Hei Suomi!"
```

## Syväluotaus:

Elixirin alkuajat juontuvat 2011 vuoteen, kun José Valim loi kielen. HTML:n jäsentämiseen Elixirillä, `Floki` on nykyään suosittu työkalu, vaikka vaihtoehtojakin, kuten `Meeseeks`, löytyy. Molemmat kirjastot tukeutuvat Elixirin ja Erlangin tehokkuuteen käsittelemään rinnakkaista suoritustapaa. `Floki`:n käyttö perustuu moitteettoman HTML:n puhdistukseen ja jäsentämiseen, ottamalla käyttöön `mochiweb` HTML-työkaluja.

## Katso Myös:

- Floki GitHub-sivu: https://github.com/philss/floki
- Elixirin viralliset dokumentit: https://elixir-lang.org/docs.html
- "Programming Phoenix" -kirja, joka sisältää lukuja HTML:n käsittelystä Elixirillä: https://pragprog.com/titles/phoenix14/programming-phoenix-1-4/
- Meeseeks-kirjasto: https://github.com/mischov/meeseeks
