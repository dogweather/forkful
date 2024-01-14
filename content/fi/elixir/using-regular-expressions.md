---
title:    "Elixir: Säännöllisten lausekkeiden käyttö"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Miksi käyttää regular expressioneja?

Regular expressionit (lyhennettynä regex tai regexp) ovat voimakas työkalu Elixir-ohjelmoijille, joiden avulla voidaan muokata ja etsiä tietyntyyppisiä merkkijonoja tekstissä. Ne voivat säästää aikaa ja vaivaa, kun etsit tiettyjä kuvioita datasta tai haluat suorittaa useita muutoksia kerralla tietyssä tekstiosassa.

## Miten käyttää regular expressioneja?

Käytämme regular expressioneja slashmerkkien (/) ja lainausmerkkien (") sisällä. Seuraava esimerkki etsii tekstistä kaikki esiintymät "elixir" ja korvaa ne sanalla "Elixir":

```Elixir
Regex.replace(~r/elixir/, "Elixir", "Tämä on esimerkki Elixir-koodista")
```

Tuloksena saadaan:

```
"Tämä on esimerkki Elixir-koodista"
```

Regular expressioneille voidaan myös antaa vaihtoehtoja käyttämällä pystyviivoja (|) sisällä sulkumerkkejä. Seuraava esimerkki etsii tekstistä kaikki esiintymät joko "Elixir" tai "Phoenix" ja korvaa ne molemmat sanalla "Elixir/Phoenix":

```elixir
Regex.replace(~r/Elixir|Phoenix/, "Elixir/Phoenix", "Tämä on esimerkki Elixir-koodista ja Phoenixista")
```

Tuloksena saadaan:

```
"Tämä on esimerkki Elixir/Phoenix-koodista ja Elixir/Phoenixista"
```

## Regular expressioneihin syvällisemmin

Regular expressionit ovat laaja aihe, ja niiden ymmärtäminen täysin vaatii paljon harjoitusta ja opiskelua. On tärkeää muistaa, että ne eivät ole vain rajoitettuja Elixiriin, vaan niitä voi käyttää useimmissa ohjelmointikielissä. Näiden ilmaisinten opetteleminen auttaa sinua parantamaan koodisi tehokkuutta ja tekemään tarkempia hakuja ja muutoksia tekstissä.

Muutamia hyviä lähteitä opiskeluun ovat:

- [Elixir regular expression -opas](https://elixir-lang.org/getting-started/regex.html)
- [Regex101](https://regex101.com/) -sivusto, joka tarjoaa käytännössä tapauksia ja selityksiä
- [Elixirin dokumentaatio regular expressioneille](https://hexdocs.pm/elixir/Regex.html)

## Katso myös

- [Elixirin virallinen verkkosivusto](https://elixir-lang.org/)
- [Regular expressionien opiskelu: vinkkejä aloittelijoille](https://medium.com/@rachelhow/learning-regular-expressions-the-easy-way-129375450c7a)
- [Video-opetusohjelma regex-opiskeluun Elixirin kanssa](https://www.youtube.com/watch?v=wz_-vbxa4NI)