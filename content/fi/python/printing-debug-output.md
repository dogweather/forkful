---
title:                "Python: Tulostaa virheenkorjaustulostus"
programming_language: "Python"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Miksi tulostaa debug-lokeja ohjelmointissa? Debug-lokit ovat olennainen osa ohjelmointia, sillä ne auttavat havaitsemaan ja korjaamaan virheitä koodissa. Tulostamalla debug-lokeja voit helposti seurata ohjelman suoritusta ja selvittää, missä mahdollinen virhe aiheuttaa ongelman.

## Miten tehdä

Debug-lokkien tulostaminen on helppoa Pythonissa. Voit käyttää sisäistä "print()" -funktiota tulostamaan haluamasi viestin. Voit myös käyttää "logging" -moduulia, joka antaa sinulle enemmän hallintaa siitä, mitä haluat tulostaa ja minne haluat sen tallentaa. Alla on esimerkkejä molemmista tavoista:

```Python
# Käyttäen print() -funktiota:
x = 10
print("x on:", x) # Tulostaa "x on: 10"

# Käyttäen logging-moduulia:
import logging
logging.basicConfig(filename='debug_loki.txt', level=logging.DEBUG) # Asettaa tason, jolla haluat tulostaa debug-lokkeja

x = 10
logging.debug("x on " + str(x)) # Tulostaa "x on 10" debug_loki.txt -tiedostoon 
```

Tässä esimerkissä käytämme "logging" -moduulia ja asetamme tason "DEBUG". Tämä tarkoittaa, että kaikki "debug()" -funktiolla tulostetut viestit tallennetaan tiedostoon nimeltä "debug_loki.txt". Voit myös asettaa tason esimerkiksi "WARNING", mikä tarkoittaa, että vain varoituksia ja tärkeitä viestejä tallennetaan.

## Syvällisempi tieto

Debug-lokit voivat auttaa paljon virheiden löytämisessä ja korjaamisessa, mutta ne voivat myös hidastaa ohjelman suoritusta, jos niitä käytetään liikaa. On tärkeää käyttää niitä vain tarvittaessa ja poistaa ne lopullisesta koodista.

Lisäksi voit myös käyttää "assert" -lausetta, joka tarkistaa koodisi tietyillä ehdoilla ja tulostaa virheen, jos jokin ei toteudu. Tämä on hyödyllinen työkalu, kun haluat varmistaa, että koodisi toimii halutulla tavalla.

## Katso myös

- Tulostaminen Pythonissa: https://www.python.org/dev/peps/pep-0201/ 
- "logging" -moduulin dokumentaatio: https://docs.python.org/3/library/logging.html 
- "assert" -lauseen dokumentaatio: https://docs.python.org/3/reference/simple_stmts.html#the-assert-statement