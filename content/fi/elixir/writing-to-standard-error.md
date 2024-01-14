---
title:                "Elixir: Kirjoittaminen standardivirheeseen"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Miksi kirjoittaa standardin virheille?

Kirjoittaminen standardin virheille (standard error) on tärkeä osa ohjelmointia ja se auttaa kehittäjiä löytämään ja korjaamaan virheitä nopeasti. Kun kehität Elixir-sovelluksia, voit käyttää tätä menetelmää virheiden jäljittämiseen ja ymmärtää, mitä ohjelma tekee.

## Miten tehdä se?

Voit kirjoittaa standardin virheille yksinkertaisesti käyttämällä `IO.puts/1` -funktiota ja välittämällä sille virheviestisi parametrina. Esimerkiksi, jos haluat näyttää käyttäjälle virheviestin, voit käyttää seuraavaa koodia:

```
Elixir
IO.puts("Tapahtui virhe: Tietoja ei löytynyt.")
```

Tämä tulostaa tekstin "Tapahtui virhe: Tietoja ei löytynyt." standardin virheille.

## Syvempi sukellus

Kun kirjoitat standardin virheille, on tärkeää huomata, että se ei pysäytä ohjelman suorittamista. Sen sijaan se vain tulostaa virheviestin ja jatkaa ohjelman suorittamista normaalisti. Tämä tekee siitä hyödyllisen työkalun virheiden jäljittämiseen ja korjaamiseen.

Lisäksi, jos haluat näyttää erityyppisiä virheviestejä, voit käyttää `IO.puts/2` -funktiota ja välittää sille toisen parametrin tulostuskanavana. Tällä tavoin voit erottaa virheviestit muista tulostuksista ja hallita niitä erikseen.

# Katso myös

- Elixirin virallinen dokumentaatio: https://elixir-lang.org/docs.html
- Virheiden hallinta Elixirissä: https://elixirschool.com/en/lessons/basics/error-handling/