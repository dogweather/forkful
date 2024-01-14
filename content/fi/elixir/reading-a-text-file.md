---
title:                "Elixir: Tekstitiedoston lukeminen"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Voit ehkä ihmetellä, miksi haluaisit lukea tekstitiedoston Elixir-ohjelmoinnilla. On monia syitä, miksi tekstitiedostojen lukeminen on hyödyllistä: voit esimerkiksi lukea ja analysoida suuria määriä dataa tai käyttää sitä osana sovellustasi.

## Kuinka Tehdä

Elixirilla tekstitiedoston lukeminen on helppoa ja tehokasta. Käytä `File.read!/1` -funktiota lukeaksesi tiedoston sisällön. Katso alla olevaa koodiesimerkkiä ja tulostetta:

```Elixir
file = File.read!("tiedosto.txt")
IO.puts file
```

Tässä koodissa luemme tiedoston nimeltä `tiedosto.txt` ja tulostamme sen sisällön. Voit myös käyttää muita `File` -moduulin funktioita, kuten `File.read/1`, `File.stream!/2` ja `File.stream/2`, lisätäksesi toimintoja tiedostojen lukemiseen.

## Syvempi Sukellus

Kun luet tekstitiedostoja Elixirilla, on tärkeää ottaa huomioon tiedoston koodaus. Voit määrittää tiedoston koodauksen `File.read!/2` ja `File.stream!/3` -funktioilla. Voit myös käyttää `File.cwd/0` -funktiota määrittääksesi nykyisen työhakemiston, josta tiedosto pitäisi lukea.

On myös hyvä huomioida, että tiedoston lukeminen on synkroninen operaatio, joka tarkoittaa sitä, että ohjelmasi pysähtyy lukemisen ajaksi. Jos haluat tehdä tiedoston lukemisen asynkronisesti, voit käyttää `Task.async/1` -funktiota ja ottaa käyttöön Elixirin oma kilta-järjestelmä.

## Katso Myös

[Elixirin Virallinen Dokumentaatio](https://hexdocs.pm/elixir/File.html)

[Elixirin Virallinen Ohjelmointiopas](https://elixir-lang.org/getting-started/introduction.html)