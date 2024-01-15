---
title:                "Yhdistävät merkkijonot"
html_title:           "Elixir: Yhdistävät merkkijonot"
simple_title:         "Yhdistävät merkkijonot"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

# Miksi: Merkkijonojen yhdistäminen Elixirissä

Merkkijonojen yhdistäminen on yleinen tehtävä ohjelmoinnissa, ja Elixirissä se sujuu helposti ja tehokkaasti. Yhdistetyt merkkijonot ovat hyödyllisiä esimerkiksi käyttöliittymien rakentamisessa, tekstimuotoisten tietojen muokkauksessa ja sisältöjen luomisessa.

Kun haluat yhdistää useita merkkijonoja yhdeksi, on Elixirin tarjoama `<>` operaattori nopein ja kätevin tapa tehdä se.

## Miten tehdä se:

Seuraavassa esimerkissä käytetään `<>` operaattoria yhdistämään kolme merkkijonoa yhdeksi:

```elixir
iex> "Tämä" <> " on" <> " esimerkki."
```
```elixir
"Tämä on esimerkki."
```

Voit myös yhdistää muuttujia merkkijonoihin, mikä tekee koodistasi joustavampaa ja dynaamisempaa. Esimerkiksi:

```elixir
iex> nimi = "Elina"
iex> "Hei, nimeni on " <> nimi <> "."
```
```elixir
"Hei, nimeni on Elina."
```

Mikäli haluat lisätä merkkijonoon myös numeerisen arvon, sinun täytyy muuttaa se ensin merkkijonoksi `to_string()` -funktiolla. Esimerkiksi:

```elixir
iex> ikä = 28
iex> "Olen " <> to_string(ikä) <> " vuotta vanha."
```
```elixir
"Olen 28 vuotta vanha."
```

## Syvällinen sukellus:

Elixirin `<>` operaattori on todella nopea, sillä se hyödyntää binäärihakemistojen tehokkuutta. Tämä tarkoittaa, että yhdistämisen aikana käytetään vähemmän muistia ja datan liikkuminen on nopeampaa kuin muissa kielissä, jotka käyttävät merkkijonojen yhdistämiseen `+` operaattoria.

Tärkeä asia huomata on myös se, että Elixirissä merkkijonot eivät ole muutettavissa, eli vanhaa merkkijonoa ei muokata uuden lisäämisen yhteydessä, vaan uusi merkkijono luodaan.

Tämä tekee Elixiristä erittäin hyvä valinta sovelluksiin, joissa työskennellään suuren määrän merkkijonojen kanssa, kuten palvelinsovelluksissa. Merkkijonojen muutokset eivät vaikuta muihin käytössä oleviin merkkijonoihin, joten on helpompi käsitellä tiettyjä muutoksia tai virheitä.

# Katso myös:

- [Elixirin virallinen verkkosivusto](https://elixir-lang.org/): Löydät täältä tietoa Elixiristä, sen syntaksista, asennuksesta ja muista tärkeistä asioista.
- [Elixir School](https://elixirschool.com/): Interaktiivinen ja kattava opas Elixirin opiskeluun ja taitojen kehittämiseen.
- [The Elixir Forum](https://elixirforum.com/): Suomenkielinen yhteisö, josta saat apua ja neuvoja Elixirin käytössä.