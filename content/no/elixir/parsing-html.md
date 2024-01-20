---
title:                "Analyse av HTML"
date:                  2024-01-20T15:31:10.860404-07:00
html_title:           "Arduino: Analyse av HTML"
simple_title:         "Analyse av HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
HTML-parsing er å tolke og analysere oppbygningen av en HTML-dokument. Programmerere gjør dette for å trekke ut data, manipulere innhold eller integrere nettsider med andre tjenester.

## How to:
Her ser vi hvordan man kan parse HTML med Elixir ved å bruke biblioteket Floki.

```elixir
# Legg til Floki til mix.exs avhengigheter
defp deps do
  [
    {:floki, "~> 0.32.0"} # Sjekk den siste versjonen
  ]
end

# Fetch og parse en HTML-side
def fetch_and_parse(url) do
  {:ok, response} = HTTPoison.get(url)
  {:ok, dok} = response.body
  |> Floki.parse()
  dok
end

# Hent ut alle linker fra en nettside
def extract_links(html) do
  html
  |> Floki.find("a")
  |> Floki.attribute("href")
end

fetch_and_parse("https://eksempel.no")
|> extract_links
|> IO.inspect
```

Resultatet vil vise en liste av linker fra den spesifiserte nettsiden.

## Deep Dive:
I gammeldagse dager brukte man regex for å parse HTML, men det var ingen god idé — HTML er for kompleks for regex. I Elixir-verdenen har vi biblioteker som Floki og meeseeks, bygget på toppen av en robust parser som heter mochiweb html.

Bruk av Floki er populært på grunn av dens jQuery-lignende syntaks, mens meeseeks kan tiltrekke de som liker den CSS-selektor tilnærmingen. Det er viktig å velge et bibliotek som passer best til dine behov.

Kjernen i problemet med parsing av HTML ligger i det faktum at HTML ikke alltid er godt strukturert. Biblioteker må derfor håndtere feil og mangler i dokumenter på en elegant måte, og det er dette Floki og meeseeks er designet for.

## See Also:
- [Floki on Hex.pm](https://hex.pm/packages/floki)
- [HTTPoison on Hex.pm](https://hex.pm/packages/httpoison)
- [Meeseeks on Hex.pm](https://hex.pm/packages/meeseeks)
- [Awesome Elixir: A curated list of amazingly awesome Elixir libraries](https://github.com/h4cc/awesome-elixir)