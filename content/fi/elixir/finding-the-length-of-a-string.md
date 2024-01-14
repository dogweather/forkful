---
title:    "Elixir: Sellisen merkkijonon pituuden löytäminen"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Miksi

Elixir on funktionaalinen ohjelmointikieli, joka on kasvattanut suosiotaan viime vuosina. Yksi sen monista hyödyllisistä ominaisuuksista on kyky käsitellä merkkijonoja, jotka ovat osa lähes jokaisen ohjelman peruslogiikkaa. Tässä kirjoituksessa käymme läpi, miten Elixirillä voi löytää merkkijonon pituuden ja miksi se on tärkeä taito ohjelmointia opetellessa.

## Miten

Merkkijonon pituuden löytäminen Elixirillä on helppoa ja nopeaa. Käytämme tätä tehdessämme `String.length`-funktiota ja annamme sille parametrina haluamamme merkkijonon. Tämän jälkeen funktio palauttaa kyseisen merkkijonon pituuden.

```Elixir
String.length("Tämä on esimerkkiteksti")
```

Tämän yksinkertaisen koodinpätkän tulostuksena näemme luvun 24, mikä kertoo, että annetussa merkkijonossa on 24 merkkiä mukana välilyöntejä. Voimme myös ottaa huomioon, että funktio laskee myös erikoismerkit, kuten pisteet ja pilkut, mukaan merkkien kokonaismäärään.

## Syventyminen

Merkkijonon pituuden löytäminen voi vaikuttaa yksinkertaiselta tehtävältä, mutta on tärkeää ymmärtää, miten tämä toimii Elixirin puolella. Funktio `String.length` käyttää taustalla `String.codepoints`-funktiota, joka jakaa merkkijonon yksittäisiin merkkeihin ja laskee sitten niiden lukumäärän. Tämä selittää myös, miksi välilyönnit ja erikoismerkit lasketaan mukaan pituuteen.

Merkit ovat myös tallennettuina sellaisina kuin ne esiintyvät Unicode-standardin mukaisesti. Tämä mahdollistaa merkkijonon pituuden laskemisen myös monikielisessä ympäristössä, jossa on käytössä esimerkiksi aasialaisia kirjoitusjärjestelmiä.

## Katso myös

- [Elixir - virallinen sivusto](https://elixir-lang.org)
- [Elixir-oppaat ja dokumentaatio](https://elixir-lang.org/getting-started.html)
- [Elixir-yhteisö ja keskustelufoorumit](https://elixirforum.com)