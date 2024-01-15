---
title:                "Vianjäljitystulostus"
html_title:           "Elixir: Vianjäljitystulostus"
simple_title:         "Vianjäljitystulostus"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi
 Aivan kuin käytämme karttaa löytääksemme tien tuntemattomassa paikassa, käytämme debug-vedostoina tietoisesti tulostettua tietoa tarkistaaksemme koodimme toiminnan. Se on olennainen työkalu löytää virheitä, seurata koodin kulkua ja kehittää parempaa ohjelmistoa.

## Miten
Debug-tulosteen luominen Elixirissä on yksinkertaista. Voit käyttää `IO.inspect/2`-funktiota tulostamaan arvoja tai muuttujia koodisi sisällä. Voit tulostaa yksittäisiä arvoja tai jopa kokonaisia rakenteita. Esimerkiksi:

```elixir
my_variable = 42
IO.inspect(my_variable)
```
#### Tulos:

`42`

Voit myös luoda monirivisiä tulosteita käyttämällä `IO.puts/1`-funktiota ja merkkijonoviittauksia:

```elixir
IO.puts("Tervetuloa!")
IO.puts("Tämä on Elixir!")
```
#### Tulos:

```
Tervetuloa!
Tämä on Elixir!
```

## Syvä sukellus
Elixirin debug-tulosteiden luominen on joustavaa ja helppoa. Voit myös käyttää `IO.inspect/2`-funktiolle annettavia lisäparametreja, kuten `pretty: true` tulostamaan kauniin muotoilun ja `colors: [syntax: :bright]` korostamaan syntaksia. Lisäksi voit käyttää `IO.inspect/3`-funktiota mahdollistaaksesi ehdollisen tulosteen, joka tulostuu vain kun ehtoa noudatetaan. Esimerkiksi:

```elixir
my_variable = "debug"
IO.inspect(my_variable, pretty: true, colors: [syntax: :bright])
```
#### Tulos:

`"debug"`

Voit myös käyttää `IO.inspect/1`-funktiota debug-tulosteiden luomiseen moduulin sisällä tai `ExUnit`-testien aikana. Muista kuitenkin poistaa kaikki debug-tulosteet ennen kuin siirrät koodin tuotantoon.

## Katso myös
- [Elixir dokumentaatio debug-tulosteiden luomisesta](https://hexdocs.pm/elixir/IO.html#inspect/1)
- [Elixir School - Debug muiden ohjelmointikielten kehittäjille](https://elixirschool.com/fi/lessons/basics/debugging/)