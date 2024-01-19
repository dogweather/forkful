---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Miten Tekstiä Haetaan ja Korvataan Pythonissa?

## Mikä & Miksi?
Tekstin haku ja korvaaminen tarkoittaa tiettyjen sanojen tai lauseiden vaihtamista toisiksi koodissa. Koodarit tekevät sen, koska he tarvitsevat tehdä koodistaan siistimmän tai käyttää eri nimistöä uudessa ympäristössä.

## Näin se tehdään:
Pythonin `re` moduuli on erittäin tehokas työkalu regular lausekkeiden (regexp) käsittelyyn.

```Python
import re

teksti = "Oulu on kaunis"
haku_nimi = "Oulu"
korvaus_nimi = "Helsinki"

muutettu_teksti = re.sub(haku_nimi, korvaus_nimi, teksti)
print(muutettu_teksti)
```
Tässä otamme syötetekstin "Oulu on kaunis" ja korvaamme sanan "Oulu" sanalla "Helsinki".

Tämä koodi tulostaa: "Helsinki on kaunis".

## Tarkempi Tarkastelu:
Pythonin `re` moduuli on ollut osana Pythonia sen alkuaikojen versiosta 1.5 lähtien, jolloin se otettiin mukaan vuonna 1997. Se tehtiin tarjotakseen Python-ohjelmoijille voimakas työkalu tekstin manipulointiin käyttäen regular lauseketta.

On myös muita tapoja tehdä tekstinkäsittelyä Pythonissa, esimerkiksi käyttämällä `replace`-metodia. Mutta `re`-moduuli on yleisin ja tehokkain tapa manipuloida tekstiä monimutkaisemmissa skenaarioissa.

Sisäisesti, kun soitat `re.sub`, Python luo `re.compile` objektin joka pitää regular lausekkeen, ja sitten kutsuu sitä `sub`-metodia. Se luo uuden lausekkeen sanasta joka on sovittunut, jossa korvaukset on tehty, ja palaa sen.

## Katso Myös:
1. Pythonin `re` moduuli: https://docs.python.org/3/library/re.html
2. Regular lausekkeet Pythonissa: https://docs.python.org/3/howto/regex.html
3. Pythonin sisäinen `re.compile`: https://docs.python.org/3/library/re.html#re.compile