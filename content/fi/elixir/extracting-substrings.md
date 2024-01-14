---
title:    "Elixir: Alimerkkijonojen erottelu"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi meidän tulisi käyttää alimerkkijonojen erottamista Elixir-ohjelmoinnissa? Alimerkkijonot voivat olla hyödyllisiä esimerkiksi tekstin käsittelyssä tai merkkijonojen jäljittämisessä. Elixirissä on monia hyödyllisiä toimintoja, jotka helpottavat alimerkkijonojen erottamista, mikä tekee siitä loistavan valinnan ohjelmointikieleksi sille, joka haluaa työskennellä merkkijonodatan kanssa.

## Miten Teet

Erityisesti Elixirillä on erittäin yksinkertainen tapa erottaa alimerkkijonoja. Voit käyttää `String.split/2` -funktiota, joka ottaa kaksi parametria, ensimmäisenä parametrina merkkijono ja toisena erottimena. Se palauttaa listan, jossa on alimerkkijonoja eroteltuna erottimella. Alla on esimerkki käytöstä:

```Elixir
string = "Tervetuloa maailmaan"
String.split(string, " ")
```

Tämä palauttaa listan `["Tervetuloa", "maailmaan"]`.

Voit myös käyttää `String.split/3` -funktiota, joka lisää kolmannen parametrin määräksi, minkä verran alimerkkijonoja haluat palauttaa:

```Elixir
string = "Hei, mitä kuuluu?"
String.split(string, " ", 2)
```

Tämä palauttaa listan `["Hei,", "mitä kuuluu?"]`.

Voit myös käyttää `String.split_at/2` -funktiota, joka jakaa merkkijonon annetusta indeksistä alkaen. Tässä on esimerkki:

```Elixir
string = "Elixir on mahtava ohjelmointikieli"
String.split_at(string, 6)
```

Tämä palauttaa listan `["Elixir ", "on mahtava ohjelmointikieli"]`.

## Syvällinen Sukellus

Merkkijonoja on mahdollista erottaa monilla eri tavoilla Elixirissä, mutta nämä esimerkit ovat vain muutamia yksinkertaisia tapoja. Voit myös käyttää säännöllisiä lausekkeita alimerkkijonojen erottamiseen tai käyttää erilaisia kirjastoja, jotka tarjoavat lisätoimintoja.

Esimerkiksi `Poison` -kirjasto tarjoaa `Poison.split/3` -funktion, joka toimii samaan tapaan kuin `String.split/3`, mutta hyväksyy myös säännöllisen lausekkeen erottimena. Tämä voi olla hyödyllistä, jos haluat hienostuneemman tavan erottaa alimerkkijonoja.

Voit myös käyttää `Regex` -moduulia ja sen `split/2` -funktiota, joka jakaa merkkijonon säännöllisen lausekkeen perusteella. Tässä on esimerkki:

```Elixir
string = "Tämä on esimerkkilause, josta haluat erottaa alimerkkijonot"
Regex.split(~r/,|\s/, string)
```

Tämä palauttaa listan `["Tämä", "on", "esimerkkilause", "josta", "haluat", "erottaa", "alimerkkijonot"]`.

Kuten näet, vaihtoehtoja on monia ja on tärkeää valita paras tapa erilaisten tarpeidesi perusteella.

## Katso Myös

- [Elixirin virallinen dokumentaatio merkkijonojen käsittelystä](https://hexdocs.pm/elixir/String.html)
- [Poison-kirjaston dokumentaatio](https://hexdocs.pm/poison/P