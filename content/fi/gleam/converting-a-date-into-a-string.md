---
title:                "Gleam: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Miksi

On monia syitä, miksi sinun kannattaisi muuntaa päivämäärä merkkijonoksi. Ehkä haluat tulostaa päivämäärän tietyn muodon mukaisesti tai tallentaa sen tietokantaan. Gleam tekee tämän prosessin helpoksi ja sujuvaksi.

# Miten tehdä

Muuntaminen päivämäärästä merkkijonoksi Gleamilla on helppoa. Käytämme tähän "to_string" -funktiota, joka ottaa parametrina päivämäärän ja palauttaa sen merkkijonona. Alla on esimerkkejä koodista ja sen tulostuksista Gleamilla.

```Gleam
let date = Date.from_string("2020-06-01", "%Y-%m-%d")
let string_date = Date.to_string(date, "%A, %d. %B %Y")

io.format("Päivämäärä merkkijonona: {}", [string_date])
// Tulostus: Päivämäärä merkkijonona: maanantai, 01. kesäkuuta 2020
```

Voit myös käyttää Gleamin "Fmt" -kirjastoa helpottamaan muotoilua. Se tarjoaa erilaisia ​​apufunktioita päivämäärien muotoilemiseen. Alla olevassa esimerkissä käytämme "fmt.rfc822" -funktiota muuntamaan päivämäärän RFC 822 -muotoon.

```Gleam
let date = Date.from_string("2020-06-01", "%Y-%m-%d")
let rfc822_date = Fmt.rfc822(date)

io.format("RFC 822 -muotoinen päivämäärä: {}", [rfc822_date])
// Tulostus: RFC 822 -muotoinen päivämäärä: Mon, 01 Jun 20 00:00:00 +0000
```

# Syvällinen sukellus

Gleam käyttää taustallaan Erlangin "calendar"-moduulia, joka tarjoaa monia erilaisia muuntoja päivämäärän ja ajan välillä. Tämän ansiosta Gleamissa on runsaasti tehokkuutta ja tarkkuutta.

Muotoilumerkkijät, kuten "%Y-%m-%d" ja "%A, %d. %B %Y", sisältävät erilaisia ​​merkkejä, jotka kertovat Gleamille, millainen tulostus halutaan. Jokainen merkki vastaa tietyille osille päivämäärää, kuten vuodelle, kuukaudelle, päivälle jne. Lisätietoja näistä löytyy Erlangin kalenterimoduulin dokumentaatiosta.

# Katso myös

- Gleamin "Date" -moduulin dokumentaatio
- Erlangin "calendar" -moduulin dokumentaatio