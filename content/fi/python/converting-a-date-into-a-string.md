---
title:    "Python: Päivämäärän muuttaminen merkkijonoksi"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Miksi konvertoida päivämäärä merkkijonoksi?

On monia syitä miksi voit haluta konvertoida päivämäärän merkkijonoksi Python-ohjelmassa. Yksi mahdollinen syy voisi olla halu näyttää päivämäärä tietyn muodon mukaan esimerkiksi käyttäjälle selkeämmin tai tallentaaksesi sen tiedostoon.

## Miten se tehdään?

Pythonissa päivämäärän konvertoiminen merkkijonoksi on suhteellisen helppoa ja vaatii vain muutaman koodirivin. Voit käyttää strftime-funktiota, joka ottaa päivämäärän ja muodon määrittelyjoukon parametreina ja palauttaa päivämäärän merkkijonona halutussa muodossa. Katso esimerkki alla olevasta koodilohkosta:

```Python
from datetime import datetime

# Luo datetime-objekti nykyisellä päivämäärällä
today = datetime.now()

# Konvertoi päivämäärän merkkijonoksi ISO-formaatissa
iso_date = today.strftime("%Y-%m-%d")
print(iso_date) # tulostaa: 2021-03-15

# Konvertoi päivämäärän merkkijonoksi kellonajan kanssa
datetime_string = today.strftime("%Y-%m-%d %H:%M:%S")
print(datetime_string) # tulostaa: 2021-03-15 12:30:00
```

Ohjelman tulos näyttäisi tältä:

```
2021-03-15
2021-03-15 12:30:00 
```

Voit myös muotoilla päivämäärän haluamallasi tavalla, esimerkiksi lisätä päivän nimen tai käyttää erilaisia aikavyöhykkeitä. strftime-funktiolla on paljon vaihtoehtoja, joten suosittelemme tarkistamaan Pythonin virallisen dokumentaation lisätietoja varten.

## Syvä sukellus

Päivämäärän konvertoiminen merkkijonoksi on osa Pythonin datetime-moduulia, joka sisältää työkaluja päivämäärän ja kellonajan käsittelyyn. Moduulin avulla voit luoda datetime-objekteja ja suorittaa erilaisia toimintoja, kuten aritmeettisia operaatioita ja muutoksia aikavyöhykkeeseen.

On myös tärkeää huomata, että päivämäärän muotoilu voi vaihdella kulttuureittain, joten on hyvä tarkistaa millaista formaattia haluat käyttää. Esimerkiksi, jos ohjelmaasi käyttävät myös ulkomaalaiset käyttäjät, on hyvä varmistaa että päivämäärän muoto on ymmärrettävä kaikille.

## Katso myös

- Pythonin virallinen datetime-dokumentaatio: https://docs.python.org/fi/3/library/datetime.html
- strftime-funktion muotoiluohjeet: https://strftime.org/
- Lisätietoa aikavyöhykkeistä Pythonissa: https://pypi.org/project/pytz/