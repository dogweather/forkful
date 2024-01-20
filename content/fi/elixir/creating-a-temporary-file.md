---
title:                "Tilapäisen tiedoston luominen"
html_title:           "Arduino: Tilapäisen tiedoston luominen"
simple_title:         "Tilapäisen tiedoston luominen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Väliaikaisten tiedostojen luominen on prosessi, jossa luodaan tiedosto, joka on tarkoitettu vain lyhytaikaiseen käyttöön. Ohjelmoijat tekevät tämän tallentaakseen tuloksia tai välivaiheita prosessin aikana, joita voidaan käyttää myöhemmin.

## Miten:
Elixirissä voimme käyttää `File`-moduulin `mktemp/1,2`-metodeja väliaikaisten tiedostojen luomiseen.
```Elixir
{:ok, path} = File.mktemp()
IO.puts path  # Prints something like: /tmp/tmp-1631-0
```
Yllä oleva koodinpalanen luo väliaikaisen tiedoston ja tulostaa sen polun.

## Syvällistä tietoa:
- Väliaikaiset tiedostot ovat olleet olemassa lähes yhtä kauan kuin itse tietokonejärjestelmät, ne ovat välttämättömiä datan käsittelyssä ja säilytyksessä.
- Elixirissä on muitakin tapoja luoda väliaikaisia tiedostoja, mutta `File.mktemp` tarjoaa nopeimman, kätevimmän ja turvallisen tavan tehdä se.
- `File.mktemp` luo tiedoston käyttöjärjestelmän väliaikaisessa hakemistossa ja palauttaa polun tiedostoon sekä tiedoston kahvan, jota voidaan käyttää tiedoston kirjoitus ja lukemisoperaatioihin.

## Katso myös:
- Elixirin virallinen dokumentaatio `File`-moduulin metodeista: https://hexdocs.pm/elixir/File.html
- Väliaikaisten tiedostojen käyttöohjeet StackOverflow'ssa: https://stackoverflow.com/questions/48719873/how-to-create-a-temp-file-in-elixir