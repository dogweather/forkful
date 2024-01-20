---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Muotoile String Pythonissa: Mitä, Miksi ja Kuinka?

## Mitä & Miksi?

Merkkijonojen interpolointi on tapa sisällyttää muuttujien arvoja suoraan merkkijonoon. Tätä käytetään, jotta voidaan tuottaa dynaamista sisältöä tai formatoida tietoja keskustelujenomaisella tavalla.

## Kuinka:
Niille, jotka ovat uusia Pythonissa tai ohjelmoinnissa, tässä on kaksi tapaa muotoilla merkkijonoja Pythonissa: `.format()`-metodi ja f-stringit.
```Python
# Esimerkki 1: .format()-metodi:
nimi = "Jaska"
viesti = "Hei, nimeni on {}.".format(nimi)
print(viesti) # Tulostaa: "Hei, nimeni on Jaska."

# Esimerkki 2: f-string:
nimi = "Jaska"
viesti = f"Hei, nimeni on {nimi}."
print(viesti) # Tulostaa: "Hei, nimeni on Jaska."
```
## Deep Dive
Merkkijonojen interpolointi on ollut olemassa pitkään useissa ohjelmointikielissä. Pythonin `.format()`-metodi esiteltiin Python 2.6:ssa ja f-stringit Python 3.6:ssa. F-stringit ovat yleensä suositumpia, koska ne ovat vähemmän monisanaisia ja nopeampia suorittaa. On kuitenkin tärkeää huomata, että joissakin tilanteissa, esimerkiksi kun merkkijonoja säilytetään erillisissä tietostossa, `.format()` saattaa olla parempi valinta.

## Katso myös
- Pythonin dokumentaatio merkkijonojen muotoilusta: https://docs.python.org/3/tutorial/inputoutput.html
- `.format()` vs f-stringit: https://realpython.com/python-f-strings/#f-strings-a-new-and-improved-way-to-format-strings-in-python  

Ota rohkeasti koodi ja leiki sen kanssa itse!