---
title:                "Elixir: Tarkistetaan onko hakemisto olemassa"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä sille, miksi haluat ehkä tarkistaa, löytyykö hakemisto Elixir-ohjelmassa. Ehkä haluat välttää virheitä myöhemmin koodissasi, jotta voit käsitellä tilanteen, jossa hakemistoa ei löydy. Tai ehkä tarvitset kyseistä hakemistoa ohjelmasi toimivuuden kannalta. Riippumatta siitä miksi haluat tarkistaa hakemiston olemassaolon, tämä blogipostaus auttaa sinua tekemään sen Elixirillä.

## Kuinka tehdä

Voit tarkistaa, löytyykö hakemisto Elixir-ohjelmassa käyttämällä `File.cwd?` -funktiota. Tämä palauttaa `true` jos hakemisto löytyy ja `false` jos sitä ei löydy. Katso alla oleva esimerkki:

```Elixir
if File.cwd?("/polku/hakemistoon") do
  IO.puts "Hakemisto löytyi!"
else
  IO.puts "Hakemistoa ei löytynyt."
end
```

Esimerkin tulostus riippuu siitä, löytyykö annetusta polusta hakemisto vai ei.

```
Hakemisto löytyi! // jos hakemisto löytyy
Hakemistoa ei löytynyt. // jos hakemistoa ei löydy
```

## Syvä sukellus

`File.cwd?` -funktio käyttää Elixirin `File` -moduulia, joka tarjoaa monia hyödyllisiä toimintoja tiedostojen ja hakemistojen käsittelyyn. Voit käyttää `File.cwd?` -funktion sijasta myös `File.dir?` -funktiota, joka palauttaa `true` jos parametrina annettu polku osoittaa hakemistoon ja `false` jos se osoittaa tiedostoon. Voit myös käyttää `File.ls` -funktiota saadaksesi listan hakemistossa olevista tiedostoista ja alihakemistoista.

## Katso myös

- [Elixirin virallinen tiedostojen käsittely dokumentaatio](https://hexdocs.pm/elixir/File.html)
- [Elixirin virallinen tiedostojen käsittely oppitunti](https://elixir-lang.org/getting-started/file-operations.html)
- [Tiedostojen ja hakemistojen käsittely Elixirissä - kirjoittanut Vicent Gozalbes](https://medium.com/erlang-battleground/working-with-files-and-directories-in-elixir-37c978edc3c8)