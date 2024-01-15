---
title:                "Verkkosivun lataaminen"
html_title:           "Elixir: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi?

Ladattujen verkkosivujen lukeminen ei ole vain internetin selailun perusosa, vaan myös tärkeä osa web-sovellusten ja -palvelujen kehitystä. Elixirillä on omat tapansa auttaa sinua verkkosivujen lataamisessa ja niiden tietojen käsittelyssä.

## Kuinka?

```Elixir
#Käyttö meille HTTP-kirjasto
defp deps do
  [
   {:httpoison, "~> 1.8"}
 ]
end

#Etsiä sivun lataaminen
HTTPoison.get("https://www.example.com")
|> case do
  {:ok, %HTTPoison.Response{status_code: 200, body: body}} -> 
    #Tulosta sivun sisältö konsolille
    IO.puts body
  {:error, %HTTPoison.Error{reason: reason}} ->
    #Jos jotain menee pieleen, tulosta virheilmoitus konsolille
    IO.puts "Virhe: #{reason}"
end
```

## Syvempi sukellus

Elixirin HTTP-kirjastoja on monia, mutta suosittelen tutustumaan HTTPoisoniin sen yksinkertaisen käytön vuoksi. Lisäksi voit käyttää Elixirin hienoa pattern matching -ominaisuutta lataamiesi verkkosivujen tietojen käsittelyyn.

Katso myös

- [Elixirin virallinen verkkosivu] (https://elixir-lang.org/)
- [HTTPoison -dokumentaatio] (https://hexdocs.pm/httpoison/HTTPoison.html)
- [Elixir Forum] (https://elixirforum.com/)