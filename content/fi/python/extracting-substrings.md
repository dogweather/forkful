---
title:                "Python: Alimerkkijonojen erottaminen"
simple_title:         "Alimerkkijonojen erottaminen"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi
Miksi haluaisit erottaa osajonon? Jos käyttäjien syötteen täytyy sisältää tiettyä mallia, kuten puhelinnumero tai sähköpostiosoite, voit käyttää osajonojen erotusta varmistaaksesi, että käyttäjän syöttö täyttää vaaditut kriteerit.

## Miten
Voit erottaa osajonot käyttämällä "substring" -funktiota tai säikeiden pilkkomismenetelmää, joka antaa sinulle osajonon tietystä merkkijonosta. Seuraavassa koodiesimerkissä käytämme "substring" -funktiota erottaaksemme ensimmäiset kolme merkkiä merkkijonosta ja tulostamme sen:

```Python
teksti = "Tämä on esimerkki"
osajonot = teksti.substring(0, 3)
print(osajonot)
```
Tämän koodin tulostus olisi:
```
Täm
```
Voit myös käyttää "split" -funktiota pilkkoaksesi merkkijonon tietyn merkin perusteella. Esimerkiksi seuraava koodi jakaa merkkijonon pisteiden kohdalla ja tulostaa jokaisen osajonon erikseen:

```Python
teksti = "Tämä.on.esimerkki"
osajonot = teksti.split(".")
print(osajonot)
```
Tämän koodin tulostus olisi:
```
["Tämä", "on", "esimerkki"]
```

## Syvempi sukellus
Voit myös käyttää säännöllisiä lausekkeita erottaaksesi osajonot haluamallasi tavalla. Säännölliset lausekkeet ovat erittäin hyödyllisiä, kun haluat erottaa monimutkaisempia osajonoja, kuten puhelinnumeroita tai sähköpostiosoitteita. Käytä "re" -moduulia ja säännöllisiä lausekkeita seuraavalla tavalla:

```Python
import re
teksti = "Minun puhelinnumeroni on (123) 456-7890."
puhelinnumero = re.search("\(\d{3}\) \d{3}-\d{4}", teksti)
print(puhelinnumero.group())
```
Tämän koodin tulostus olisi:
```
(123) 456-7890
```
Tässä käytetään säännöllistä lauseketta, joka etsii tietyn mallisen puhelinnumeron merkkijonossa.

## Katso myös
- [Pythonin merkkijonomenetelmät](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Säännölliset lausekkeet](https://docs.python.org/3.7/library/re.html)
- [Täydellinen opas Pythonin osajonojen erotukseen](https://www.geeksforgeeks.org/python-extracting-nth-key-in-given-string/)