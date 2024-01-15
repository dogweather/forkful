---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Fish Shell: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi yhdistellä merkkijonoja Fish Shellillä? Se voi olla hyödyllistä, kun luodaan monimutkaisempia ja muuttuvia merkkijonoja, esimerkiksi tulostettaessa tietojärjestelmän tilatietoja tai luotaessa dynaamisia komentokieliä.

## Näin teet sen

Fish Shellillä stringien (merkkijonojen) yhdistäminen on helppoa ja intuitiivista. Voit käyttää ``echo`` komentoa tai Fish Shellin laajennettua muotoilua (``string.format``) yhteisiin yhdistelytarpeisiin.

```
# Luodaan yksinkertainen merkkijono
set greeting "Hei!"

# Liitetään siihen toinen merkkijono
echo $greeting "Kuinka voit tänään?"

# Output: Hei! Kuinka voit tänään?
```

Voit myös käyttää ``string.join`` -komennon avulla concatenate useita merkkijonoja yhdeksi. Tämä on erityisen kätevää, kun haluat yhdistää elementtejä listasta tai taulukosta.

```
# Luodaan lista lomakohteista
set destinations Sveitsi Ranska Italia Saksa

# Yhdistetään listan elementit välilyönnillä
set trip "Matkustetaan maahan:" (string.join " " $destinations)

# Tulostetaan tulos
echo $trip

# Output: Matkustetaan maahan: Sveitsi Ranska Italia Saksa
```

## Deep Dive

Fish Shellin laajennettu merkkijonojen käsittely tarjoaa monia hyödyllisiä työkaluja yhdistelyä varten. Voit esimerkiksi käyttää ``string.split`` -komennon avulla jakaa merkkijono osiin haluamasi merkkijonon perusteella.

```
# Luodaan IP-osoite merkkijono
set ip 192.168.1.1

# Erotellaan IP-osoite osiin pisteiden perusteella
set ip_parts (string.split "." $ip)

# Tulostetaan osat
echo $ip_parts[1] # Output: 192
echo $ip_parts[2] # Output: 168
echo $ip_parts[3] # Output: 1
echo $ip_parts[4] # Output: 1
```

Voit myös käyttää Fish Shellin sisäänrakennettua matematiikkalaskimia merkkijonojen käsittelyyn. Tämä on hyödyllistä esimerkiksi, kun haluat lisätä tai vähentää numeroita merkkijonon sisällä.

```
# Luo numeromerkkijono
set number_string "100"

# Muutetaan merkkijono numeroksi ja lisätään siihen 50
set result (+ (string.to-int $number_string) 50)

# Tulostetaan tulos
echo $result

# Output: 150
```

## Katso myös

- [Fish Shellin virallinen dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Fish Shellin GitHub-sivut](https://github.com/fish-shell/fish-shell)