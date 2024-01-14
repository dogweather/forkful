---
title:                "Elixir: Tekstitiedoston kirjoittaminen"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kirjoittaisit tekstitiedoston Elixirillä? Yksi syy voi olla tiedon tallentaminen ja jakaminen muiden ohjelmien ja käyttäjien kanssa. Lisäksi Elixirin avulla tekstitiedoston käsittely ja muokkaaminen on helppoa ja tehokasta.

## Miten

Aloitetaan luomalla uusi tekstitiedosto Elixirillä. Voit tehdä tämän komennolla ```File.write("tiedosto.txt", "Tässä on esimerkki tekstitiedostosta")```. Tämä luo uuden tiedoston nimeltään "tiedosto.txt" ja tallentaa siihen annetun merkkijonon. Voit myös lukea tiedoston sisällön komennolla ```File.read("tiedosto.txt")```.

Jos haluat lisätä tiedostoon uuden rivin, voit käyttää komentoa ```File.append("tiedosto.txt", "Uusi rivi")```. Tällä tavalla voit jatkuvasti lisätä ja muokata tiedostoa tarpeidesi mukaan.

Voit myös luoda ja muokata CSV-tiedostoja Elixirillä. Tämä onnistuu esimerkiksi CSV-kirjaston avulla.

## Syvällinen sukellus

Voit käyttää Elixirin IO-moduulia luomaan ja muokkaamaan tiedostoja. IO-moduulilla on kätevä set of funktioita, kuten ```IO.write``` ja ```IO.read```, jotka helpottavat tiedoston käsittelyä. Voit myös käyttää File-moduulia CSV-tiedostojen käsittelyyn, joten sinun ei tarvitse hankkia erillisiä kirjastoja.

Kun olet valmis, muista aina sulkea tiedosto lopuksi komennolla ```File.close(file)```.

## Katso myös

- Elixirin IO-moduuli: https://hexdocs.pm/elixir/IO.html
- CSV-kirjasto: https://hexdocs.pm/csv/readme.html
- File-moduulin dokumentaatio: https://hexdocs.pm/elixir/File.html