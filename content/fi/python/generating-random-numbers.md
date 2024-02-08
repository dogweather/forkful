---
title:                "Satunnaislukujen generointi"
aliases:
- fi/python/generating-random-numbers.md
date:                  2024-01-27T20:35:24.551513-07:00
model:                 gpt-4-0125-preview
simple_title:         "Satunnaislukujen generointi"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Satunnaislukujen tuottaminen tarkoittaa sellaisten lukujen luomista, joita ei voida kohtuullisesti ennakoida paremmin kuin sattuman kautta, mikä on oleellista simulaatioiden, pelien ja turva-algoritmien kehittämisessä. Ohjelmoijat tekevät tämän tuodakseen ennalta-arvaamattomuutta tai simuloidakseen todellisia ilmiöitä sovelluksissaan.

## Kuinka:

Python tarjoaa `random`-moduulin, joka auttaa luomaan satunnaislukuja eri käyttötarkoituksiin. Näin pääset alkuun:

1. **Moduulin tuonti**
    ```Python
    import random
    ```

2. **Satunnaisen kokonaisluvun tuottaminen**
    Minkä tahansa kahden luvun väliltä.
    ```Python
    random_kokonaisluku = random.randint(1, 10)
    print(random_kokonaisluku)
    ```
    Esimerkkituloste: `7`

3. **Liukuluvun tuottaminen**
    0:n ja 1:n välillä.
    ```Python
    random_liukuluku = random.random()
    print(random_liukuluku)
    ```
    Esimerkkituloste: `0.436432634653`

    Jos tarvitset liukuluvun eri alueelta, kertolaskulla:
    ```Python
    random_liukuluku_alue = random.random() * 5  # 0:sta 5:een
    print(random_liukuluku_alue)
    ```
    Esimerkkituloste: `3.182093745`

4. **Satunnaisen elementin valitseminen listasta**
    ```Python
    tervehdykset = ['Hello', 'Hi', 'Hey', 'Hola', 'Bonjour']
    print(random.choice(tervehdykset))
    ```
    Esimerkkituloste: `Hola`

5. **Listan sekoittaminen**
    Täydellinen korttipeleihin tai mihin tahansa sovellukseen, joka tarvitsee satunnaistaa järjestystä.
    ```Python
    numerot = list(range(10))
    random.shuffle(numerot)
    print(numerot)
    ```
    Esimerkkituloste: `[2, 5, 0, 4, 9, 8, 1, 7, 6, 3]`

## Syväsukellus

Pythonin `random`-moduuli käyttää pseudosatunnaislukugeneraattoria (PRNG), tarkemmin Mersenne Twister -algoritmia, joka soveltuu yleiskäyttöön mutta ei kryptografisiin tarkoituksiin sen ennustettavuuden vuoksi, jos tarpeeksi monia tulosteita havainnoidaan. Python 3.6:ssa esitelty `secrets`-moduuli tarjoaa paremman vaihtoehdon kryptografisesti vahvojen satunnaislukujen tuottamiseen, mikä on erityisen hyödyllistä turvallisuusherkissä sovelluksissa. Esimerkiksi turvallisen, satunnaisen tokenin tuottaminen salasanan resetointilinkkiä varten:

```Python
import secrets
token = secrets.token_hex(16)
print(token)
```

Historiallisesti todella satunnaisten lukujen tuottaminen on ollut haaste tietojenkäsittelyssä, varhaiset menetelmät luottivat fyysisiin ilmiöihin tai manuaalisesti syötettyihin siemeniin. Algoritmien, kuten Mersenne Twisterin (käytössä oletusarvoisesti Pythonin `random`-moduulissa ainakin viimeisimpään tietooni 2023 mennessä), kehitys ja omaksuminen merkitsivät merkittävää edistystä. Kuitenkin jatkuvasti turvallisempien ja tehokkaampien algoritmien etsintä on johtanut `secrets`-moduulin sisällyttämiseen kryptografiaan liittyviä tehtäviä varten. Tämä kehitys heijastaa turvallisuuden kasvavaa merkitystä ohjelmistokehityksessä ja tarvetta vahvempaan satunnaisuuteen sovelluksissa, jotka vaihtelevat salauksesta turvallisen tokenin luomiseen.
