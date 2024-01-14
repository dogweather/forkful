---
title:                "Elixir: Html:n jäsentäminen"
simple_title:         "Html:n jäsentäminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi
HTML-analyysin ymmärtäminen on tärkeää verkkosivustojen kehittämisessä ja tietojen hankkimisessa verkosta. Tässä blogikirjoituksessa käymme läpi, kuinka voit käyttää Elixir-kieltä HTML-analyysiin ja sen hyödyistä.

## Kuinka
### Yksinkertainen HTML:n analyysi käyttäen Elixir-kieltä
```Elixir
html = "<html><body><h1>Tervetuloa</h1><p>Tämä on esimerkkiteksti</p></body></html>"
parsed_html = Floki.parse(html)
IO.inspect(parsed_html) # [{:html,[{},{:body,[{},{:h1,[{},[{:raw_text,"Tervetuloa"}]]},{:p,[{},[{:raw_text,"Tämä on esimerkkiteksti"}]]}]}]}]
```
parsed_html palauttaa listan tageja ja niiden sisällä olevan sisällön. Tämä helpottaa tarvittavan tiedon eristämistä ja käsittelyä.

### Tietojen poimiminen HTML:stä
```Elixir
title = Floki.find(parsed_html, "h1") |> Floki.text |> List.first
IO.puts(title) # Tervetuloa
```
Flokin avulla voit käyttää CSS-valitsimia löytääksesi tietyn tiedon halutuista tageista. Tässä tapauksessa, poimimme "h1" tagin sisällön ja tulostamme sen käyttäen IO.moduulia.

## Syvemmälle
HTML:n analysointi Elixir-kielen avulla antaa sinulle valtavan määrän mahdollisuuksia käsitellä ja käyttää verkkosivustojen tietoja. Voit myös käyttää lisämoduuleja, kuten Hound, automatoidaksesi HTML-sivujen testausta.

## Katso myös
- [Floki:n dokumentaatio](https://hexdocs.pm/floki/Floki.html)
- [Hound Github-sivu](https://github.com/HashNuke/hound)
- [Elixir droplet - tietoja HTML-dataa käsittelevästä Elixir-ohjelmasta](http://www.creativedroplets.com/html-parsing-with-elixir/)