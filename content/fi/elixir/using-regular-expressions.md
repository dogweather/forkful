---
title:    "Elixir: Säännöllisten ilmaisujen käyttö"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi

Regular expressionien käyttö on erittäin hyödyllistä, kun halutaan löytää ja muokata tietoa tekstimuotoisesta datasta nopeasti ja tehokkaasti.

## Kuinka

Regular expressionit ovat osa monia ohjelmointikieliä, mukaan lukien Elixir. Niitä käytetään merkkijonojen käsittelyyn ja manipulointiin. Alla on muutamia esimerkkejä regular expressionien käytöstä Elixirissä:

```
Elixir -regex "hello|world"
```

Tämä haku löytäisi kaikki esiintymät sanoista "hello" tai "world" tekstissä.

```
Elixir -regex "(\\d+)\\s*(\\w+)"
```

Tämä haku löytäisi numeron ja sitä seuraavan sanan missä tahansa tekstissä. Esimerkiksi jos teksti olisi "123 hello" niin haku palauttaisi "123 hello".

Elixirin regular expressionit tukevat myös monia eri vaihtoehtoja ja metakaraktereita, jotka tekevät niiden käytöstä joustavaa ja voimakasta. Voit oppia lisää Elixirin regular expressioneista [täältä](https://hexdocs.pm/elixir/Regex.html).

##Syväsukellus

Regular expressioneiden lukeminen ja ymmärtäminen voi olla vaikeaa aluksi, mutta harjoittelu tekee mestarin. On tärkeää ymmärtää, että regular expressionit ovat vain merkkijonoja, joten ne voivat tuntua vaikeilta luettavilta ja epämääräisiltä. Paras tapa oppia niitä on kokeilemalla erilaisia hakuja ja näkemällä, miten ne toimivat eri skenaarioissa.

On myös hyödyllistä muistaa, että regular expressionien käyttö ei ole ainoa tapa käsitellä merkkijonoja Elixirissä. On olemassa muita vaihtoehtoja, kuten String-moduulin funktiot, jotka voivat joskus olla parempi ratkaisu ongelmaan.

## Katso myös

- [Elixirin virallinen Regex-dokumentaatio](https://hexdocs.pm/elixir/Regex.html)
- [Visual Regex: Interaktiivinen työkalu regexien harjoitteluun](https://www.debuggex.com/)
- [Regex101: Sivusto, joka opettaa regexien rakentamista ja testaamista](https://regex101.com/)