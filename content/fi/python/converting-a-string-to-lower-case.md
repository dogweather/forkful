---
title:                "Python: Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi
Miksi kannattaisi muuntaa merkkijono pieniksi kirjaimiksi?

## Kuinka
```Python
# Luodaan esimerkki merkkijono
teksti = "Tämä On Esimerkki"

# Muutetaan teksti pieniksi kirjaimiksi
pieniksi_kirjaimiksi = teksti.lower()

# Tulostetaan muunnettu merkkijono
print(pieniksi_kirjaimiksi)

# Output:
# tämä on esimerkki
```

## Syvällinen sukellus
Merkkijonon muuntaminen pieniksi kirjaimiksi on hyödyllinen toiminto, erityisesti silloin kun vertailuja tehdään merkkijonojen välillä. Pieniksi muunnetut merkkijonot ovat helpommin vertailtavissa ja näin ollen tiedon löytäminen ja koodin suoritus nopeutuu. Tämä toiminto on myös hyödyllinen, mikäli halutaan varmistaa, että käyttäjän syöttämät merkit ovat yhtenäisessä muodossa.

## Katso myös
- [Pythonin string -moduulin dokumentaatio](https://docs.python.org/3/library/string.html)
- [Miten vertailla merkkijonoja Pythonissa](https://www.freecodecamp.org/news/how-to-compare-strings-in-python/)
- [Kuinka muuntaa merkkijonoja eri kielille Pythonissa](https://stackoverflow.com/questions/39104063/python-convert-string-to-another-language)