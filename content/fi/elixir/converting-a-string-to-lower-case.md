---
title:                "Elixir: Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Yksi yleisimmistä ohjelmointitehtävistä on muuttaa merkkijono pienikirjaimiseksi. Tämä voidaan tehdä useilla eri ohjelmointikielillä, mutta tänään keskitymme Elixirin tapaan muuttaa merkkijono pienikirjaimiseksi.

## Kuinka

Elixirissa merkkijonon muuttaminen pienikirjaimiseksi on hyvin yksinkertaista käyttämällä "String.downcase" -funktiota. Se ottaa argumenttina merkkijonon ja palauttaa version, jossa kaikki kirjaimet ovat pienikirjaimisia. Alla on esimerkki:

```Elixir
String.downcase("TÄMÄ ON MERKKIJONO!") 
```

Tulos tulee olemaan:

```Elixir
"tämä on merkkijono!"
```

Voit myös muuttaa merkkijonon ensimmäisen kirjaimen pienikirjaimiseksi käyttämällä "String.capitalize" -funktiota.

```Elixir
String.capitalize("merkkijono") 
```

Tulos tulee olemaan:

```Elixir
"Merkkijono"
```

## Syvempi syvennys

Elixirin "String.downcase" -funktio käyttää Unicode-tietokantoja, jotta se voi käsitellä monikielisiä merkkijonoja ja erikoismerkkejä oikein. Tämä tarkoittaa, että voit muuttaa minkä tahansa merkkijonon pienikirjaimiseksi, olipa se sitten suomen, ruotsin tai minkä tahansa muun kielen kielellä.

Jos haluat muuttaa vain osan merkkijonosta pienikirjaimiseksi, voit käyttää "String.slice" ja "String.downcase" -funktioita yhdessä. Alla on esimerkki, jossa muutamme vain ensimmäisen sanan pienikirjaimiseksi.

```Elixir
String.slice("TÄMÄ ON MERKKIJONO!", 0..3) |> String.downcase
```

Tulos tulee olemaan:

```Elixir
"tämä ON MERKKIJONO!"
```

## Katso myös

- Elixirin virallinen dokumentaatio merkkijonojen käsittelystä: https://hexdocs.pm/elixir/String.html
- Elixirin oppimateriaali suomeksi: https://terokarvinen.com/2020/elixir-tiivis-opas-terokarvinenfi-luku0/
- Elixirin viralliset verkkosivut: https://elixir-lang.org/