---
title:                "HTML:n jäsennys"
date:                  2024-02-03T19:11:46.961334-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML:n jäsennys"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTML:n jäsentäminen Elixirissä tarkoittaa tietojen poimimista HTML-dokumenteista. Ohjelmoijat tekevät tämän ohjelmallisesti vuorovaikuttaakseen verkkosivujen kanssa, kaapiakseen dataa tai automatisoidakseen verkkovuorovaikutuksia, mahdollistaen sovellusten ymmärtää ja hyödyntää verkkosisältöä dynaamisesti.

## Kuinka:

Elixir, sen vankkumattoman rinnakkaisuuden mallin ja funktionaalisen ohjelmointiparadigman kanssa, ei sisällä sisäänrakennettuja HTML-jäsentämisen valmiuksia. Voit kuitenkin käyttää suosittuja kolmannen osapuolen kirjastoja kuten `Floki` tähän tarkoitukseen. Floki tekee HTML-jäsentämisestä intuitiivista ja tehokasta, hyödyntäen Elixiriä mallin sopivuudesta ja putkituksesta.

Lisää ensin Floki mix.exs riippuvuuksiisi:

```elixir
defp deps do
  [
    {:floki, "~> 0.31.0"}
  ]
end
```

Suorita sitten `mix deps.get` asentaaksesi uuden riippuvuuden.

Nyt jäsentäkäämme yksinkertainen HTML-merkkijono datan poimimiseksi. Etsitään otsikoita `<h1>`-tageista:

```elixir
html_content = """
<html>
  <body>
    <h1>Hello, Elixir!</h1>
    <h1>Toinen Otsikko</h1>
  </body>
</html>
"""

titles = html_content
         |> Floki.find("h1")
         |> Floki.text()

IO.inspect(titles)
```

**Esimerkkituloste:**

```elixir
["Hello, Elixir!", "Toinen Otsikko"]
```

Sukeltaaksemme syvemmälle, sanotaan että haluat poimia linkit (`<a>`-tagit) niiden href-attribuuttien kanssa. Tässä kuinka voit saavuttaa sen:

```elixir
html_content = """
<html>
  <body>
    <a href="https://elixir-lang.org/">Elixiriä Virallisesti</a>
    <a href="https://hexdocs.pm/">HexDocs</a>
  </body>
</html>
"""

links = html_content
        |> Floki.find("a")
        |> Enum.map(fn({_, attrs, [text]}) -> {text, List.keyfind(attrs, "href", 0)} end)
        
IO.inspect(links)
```

**Esimerkkituloste:**

```elixir
[{"Elixiriä Virallisesti", {"href", "https://elixir-lang.org/"}}, {"HexDocs", {"href", "https://hexdocs.pm/"}}]
```

Tämä lähestyminen mahdollistaa HTML-dokumenttien tehokkaan navigoinnin ja jäsentämisen, helpottaen verkkodatan poiminta- ja manipulointitehtäviä Elixir-sovelluksissa.
