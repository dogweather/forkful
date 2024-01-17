---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Elixir: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?
Kirjoittaminen teksti tiedostoon on yksinkertainen tapa tallentaa tietoa, jota voidaan käyttää myöhemmin ohjelmoinnissa. Koodaajat käyttävät teksti tiedostoja tallentaakseen esimerkiksi käyttäjän syötteitä, ohjelman loki tietoja tai muita muuttujia.

## Kuinka:
```Elixir
File.write("teksti.txt", "Tervetuloa") 
```
Tämä koodi luo teksti tiedoston nimeltä "teksti.txt" ja tallentaa siihen tekstin "Tervetuloa". Voit vaihtaa tekstin ja tiedoston nimen halutessasi. Voit myös käyttää `File.append` komentoa lisätäksesi uutta tietoa olemassa olevaan teksti tiedostoon.

## Syvempi sukellus:
Teksti tiedostojen luominen ja muokkaaminen on ollut perusosa ohjelmoinnissa jo vuosikymmenien ajan. Kyseessä on yksinkertainen, mutta tehokas tapa tallentaa ja käsitellä tietoa. On myös useita muita tapoja tallentaa tietoa, kuten tietokantoja tai taulukoita, mutta teksti tiedostot ovat edelleen erittäin hyödyllisiä monissa tilanteissa.

## Katso myös:
- [Elixirin viralliset dokumentaatiot tiedoston käsittelystä](https://hexdocs.pm/elixir/File.html)
- [Elixirin viralliset dokumentaatiot tiedoston käsittelystä](https://hexdocs.pm/elixir/File.html)
- [Elixirin viralliset dokumentaatiot teksti tiedostojen lukemisesta](https://hexdocs.pm/elixir/File.html#content)