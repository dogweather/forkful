---
title:    "Elixir: Tekstin etsiminen ja korvaaminen"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Miksi: Miksi käyttäisit hakemista ja korvaamista tekstissä?

Hakeminen ja korvaaminen eli etsi ja korvaa ovat yleisiä toimintoja ohjelmoinnissa. Ne mahdollistavat tietyn tekstin etsimisen ja sen korvaamisen toisella tekstillä. Tämä voi säästää aikaa ja vaivaa, erityisesti silloin kun työskentelet suurilla tekstimäärillä. 

## Miten: Esimerkkejä koodin ja tulosteen kanssa

```Elixir
# Etsi ja korvaa "Hei" tekstillä "Moikka"
string = "Hei! Kuinka voit tänään?"
string = String.replace(string, "Hei", "Moikka")
# string == "Moikka! Kuinka voit tänään?"
```

```Elixir
# Etsi ja korvaa kaikki numerot tekstillä "Numero"
string = "Haluaisin varata pöydän neljälle hengelle."
string = String.replace(string, ~r/[0-9]+/, "Numero")
# string == "Haluaisin varata pöydän Numero hengelle."
```

```Elixir
# Etsi ja korvaa ei-tunnistetavat merkit tyhjällä tekstillä
string = "Tämä teksti sisältää erikoismerkkejä!"
string = String.replace(string, ~r/[^[:alnum:]\s]/, "")
# string == "Tämä teksti sisältää erikoismerkkejä"
```

## Syvempi sukellus: Lisätietoa hakemisesta ja korvaamisesta tekstissä

Hakeminen ja korvaaminen ovat yleisiä toimintoja, mutta Elixirilla voi tehdä niitä monilla eri tavoilla. String-moduulilla on erilaisia funktioita, jotka tarjoavat vaihtoehtoisia tapoja käsitellä tekstiä. Lisäksi Elixirin mukana tuleva Regex-moduuli tarjoaa tehokkaan ja tarkemman tavan käsitellä tekstiä säännöllisten lausekkeiden avulla.

On myös tärkeää huomata, että String.replace ei muuta alkuperäistä tekstiä vaan palauttaa uuden merkkijonon korvatulla tekstillä. Jos haluat muuttaa alkuperäistä merkkijonoa, täytyy tallentaa palautettu arvo uuteen muuttujaan.

## Katso myös

- [Elixir String-moduuli](https://hexdocs.pm/elixir/String.html)
- [Elixir Regex-moduuli](https://hexdocs.pm/elixir/Regex.html)
- [Säännölliset lausekkeet Elixirissa](https://elixirschool.com/en/lessons/advanced/regex/)
- [Hakeminen ja korvaaminen Rubylla](https://www.digitalocean.com/community/tutorials/how-to-use-gsub-and-gsub-with-ruby)
- [Regex opas](https://www.regular-expressions.info/)