---
title:    "Elixir: Merkkijonon kirjoittaminen isoilla kirjaimilla"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Joku saattaa kysyä: miksi ylipäätään täytyy koodata merkkijonon kirjaimien muuttaminen isoksi? Vastaus on yksinkertainen: joskus se on tarpeen tiettyjen ohjelmointitehtävien suorittamiseen. Esimerkiksi kun käsitellään käyttäjän syöttämää dataa, saattaa olla tarpeen muuttaa merkkijonon alku kirjaimella isoksi, jotta se vastaa tietyntyyppistä tietokantaan tallennettua dataa. Tämä yksi yleinen esimerkki siitä, miksi merkkijonon kirjainten muuttaminen isoksi voi olla hyödyllistä.

## Miten tehdä se

Jos käytät Elixiriä, voit helposti kääntyä sisäänrakennettuun `String.capitalize/1` -toimintoon, jolla voit muuttaa merkkijonon ensimmäisen kirjaimen isoksi. Tämän funktion käyttäminen on melko suoraviivaista ja seuraava koodiesimerkki näyttää, miten se tapahtuu Elixirin `iex`-komentotulkin kautta:

```Elixir
iex> String.capitalize("kävin eilen kaupassa")
"Kävin eilen kaupassa"
```

Huomaa, että vain merkkijonon ensimmäinen kirjain muuttuu isoksi. Voit myös käyttää `String.capitalize/2` -funktiota, joka ottaa toisen argumentin sisään, jos haluat muuttaa jonkin muun kirjaimen isoksi. Seuraava esimerkki näyttää, kuinka voit käyttää tätä toista argumenttia muuttaaksesi ensimmäisen tilalle kolmannen kirjaimen isoksi:

```Elixir
iex> String.capitalize("kävin eilen kaupassa", 3)
"käviN eilen kaupassa"
```

## Syvemmän sukelluksen tekeminen

Jos haluat syventyä Elixirissä merkkijonon kirjainten muuttamiseen isoksi, voit tutustua tarkemmin `String.capitalize/1` -funktion lähdekoodiin. Voit löytää sen Elixirin dokumentaatiosta tai etsiä GitHubista. Tämä auttaa sinua ymmärtämään, miten toiminto toimii taustalla ja miten voit räätälöidä sitä tarpeidesi mukaan.

## Katso myös

- [Elixirin dokumentaatio merkkijonon käsittelystä](https://hexdocs.pm/elixir/String.html)
- [Elixirin GitHub-repositorio](https://github.com/elixir-lang/elixir)