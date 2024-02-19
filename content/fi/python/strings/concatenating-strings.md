---
aliases:
- /fi/python/concatenating-strings/
date: 2024-01-20 17:35:19.755429-07:00
description: "Yhdist\xE4mme merkkijonoja luodaksemme pidempi\xE4 tekstej\xE4. Ohjelmoijat\
  \ tarvitsevat sit\xE4 k\xE4ytt\xF6liittymien tekstien, logiviestien ja datan k\xE4\
  sittelyn yhteydess\xE4."
lastmod: 2024-02-18 23:09:07.175107
model: gpt-4-1106-preview
summary: "Yhdist\xE4mme merkkijonoja luodaksemme pidempi\xE4 tekstej\xE4. Ohjelmoijat\
  \ tarvitsevat sit\xE4 k\xE4ytt\xF6liittymien tekstien, logiviestien ja datan k\xE4\
  sittelyn yhteydess\xE4."
title: "Merkkijonojen yhdist\xE4minen"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Yhdistämme merkkijonoja luodaksemme pidempiä tekstejä. Ohjelmoijat tarvitsevat sitä käyttöliittymien tekstien, logiviestien ja datan käsittelyn yhteydessä.

## How to: (Kuinka tehdä:)
```Python
# Yksinkertainen esimerkki
tervehdys = "Hei "
nimi = "Maija"
viesti = tervehdys + nimi
print(viesti)  # Tulostuu: "Hei Maija"

# Python 3.6+ f-string merkkijono
ika = 30
kuvaus = f"{nimi} on {ika} vuotta vanha."
print(kuvaus)  # Tulostuu: "Maija on 30 vuotta vanha."

# join()-funktiolla
harrastukset = ["ohjelmointi", "pyöräily", "lautapelit"]
luettelo = ", ".join(harrastukset)
print(f"Maijan harrastukset: {luettelo}.")
# Tulostuu: "Maijan harrastukset: ohjelmointi, pyöräily, lautapelit."
```

## Deep Dive (Syväsukellus)
Merkkijonon yhdistämiselle on monia tapoja. Historiallisesti "+"-operaattoria on käytetty pitkään. Se on nopea ja helppo merkkijonoille, jotka on tiedossa jo kirjoitushetkellä.

Python 2:ssa käytettiin "%" operaattoria merkkijonojen muotoiluun, mutta Python 3 suosii `format()`-metodia ja f-stringeä (Python 3.6+), koska ne ovat nopeampia ja helpompia lukea.

Suorituskyvyltään `.join()` on tehokkaampi suurille merkkijonoille tai silloin, kun yhdistellään listan merkkijonoja. Se käyttää vähemmän muistia, koska luo vain yhden uuden merkkijonon toisin kuin `+`-operaattori, joka luo uuden väliaikaisen merkkijonon joka välissä.

## See Also (Katso Myös)
- Pythonin dokumentaatio merkkijonon käsittelystä: https://docs.python.org/3/library/string.html
- Pythonin f-string dokumentaatio: https://docs.python.org/3/reference/lexical_analysis.html#f-strings
- W3Schools Python String Concatenation: https://www.w3schools.com/python/python_strings_concatenate.asp
