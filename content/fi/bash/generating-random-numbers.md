---
title:                "Satunnaislukujen generointi"
aliases:
- fi/bash/generating-random-numbers.md
date:                  2024-01-27T20:32:57.432945-07:00
model:                 gpt-4-0125-preview
simple_title:         "Satunnaislukujen generointi"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Satunnaislukujen tuottaminen Bashissa tarjoaa keinon tuoda epävarmuutta skripteihin, mikä on olennaista tehtävissä kuten turvallisten salasanojen generointi, datan simulointi tai pelien ohjelmointi. Ohjelmoijat hyödyntävät tätä kykyä lisätäkseen vaihtelua skripteihinsä tai testatakseen ohjelmiaan monenlaisissa satunnaisesti generoiduissa olosuhteissa.

## Kuinka:
Bashissa `$RANDOM`-muuttuja on se mihin turvaudutaan satunnaislukujen tuottamisessa. Joka kerta kun viittaat siihen, Bash tarjoaa pseudosatunnaisen kokonaisluvun väliltä 0 - 32767. Tutkitaan joitakin käytännön esimerkkejä:

```Bash
# $RANDOM:n peruskäyttö
echo $RANDOM

# Satunnaisluvun generointi määritellyllä välillä (tässä 0-99)
echo $(( RANDOM % 100 ))

# "Turvallisemman" satunnaisluvun generointi, sopii salasanoihin tai avaimiin
# Käyttäen /dev/urandomia od-komennon kanssa
head -c 8 /dev/urandom | od -An -tu4

# RANDOMin alustaminen toistettavuuden saavuttamiseksi
RANDOM=42; echo $RANDOM
```

Esimerkkituloste (huomaa: todellinen tuloste vaihtelee, koska luvut ovat satunnaisia):
```Bash
16253
83
3581760565
17220
```

## Syväluotaus
Mekanismi Bashin `$RANDOM`n taustalla tuottaa pseudosatunnaislukuja, mikä tarkoittaa, että ne noudattavat algoritmia ja ovat teoriassa ennustettavissa - potentiaalinen turvallisuusriski sovelluksille, jotka vaativat aitoa arvaamattomuutta. Nykyaikaiset kryptografiset sovellukset vaativat yleensä satunnaisuutta, joka on johdettu fyysisistä ilmiöistä tai erityisesti satunnaisen datan tuottamiseen suunnitellusta laitteistosta, kuten Linuxin `/dev/urandom` tai `/dev/random`, jotka keräävät ympäristön kohinaa.

Satunnais- tai ei-turvallisuuskriittisissä tehtävissä `$RANDOM` riittää ja tarjoaa yksinkertaisuuden etuna. Kuitenkin kryptografisiin tarkoituksiin tai kun satunnaisuuden laatu on kriittistä, kehittäjien tulisi kääntyä muiden työkalujen ja kielten puoleen, jotka on suunniteltu kryptografian näkökulmasta mielessä pitäen, kuten OpenSSL tai ohjelmointikielet, joilla on vankat satunnaislukugeneraattorikirjastot.

Vaikka Bashin `$RANDOM` toimii tarkoituksessaan skripteissä, jotka vaativat perussatunnaislukuja, sen rajoitukset tulisi ohjata kehittäjiä kohti vahvempia ratkaisuja sovelluksissa, joissa satunnaisuuden laatu tai turvallisuus on merkityksellistä.
