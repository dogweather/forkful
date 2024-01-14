---
title:                "Python: Alimerkkijonojen erottelu"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi?

Substringien ekstrahointi on tärkeä osa Python-ohjelmointia, sillä se mahdollistaa tekstimuotoisen datan prosessoinnin ja analysoinnin. Se on erityisen hyödyllistä kun halutaan etsiä tiettyjä sanoja tai lauseita pitkistä tekstiketjuista.   

## Miten?

Substringien ekstrahointi on helppoa Pythonissa. Alla on esimerkkejä koodista ja siihen liittyvästä tulosteesta.

```Python
# Yksinkertainen esimerkki substringin ekstrahoinnista merkkijonosta
teksti = "Tämä on esimerkkiteksti"
substringi = teksti[5:] # Ekstrahoidaan "on esimerkkiteksti"
print(substringi) # Tulostaa "on esimerkkiteksti"

# Ekstrahoidaan merkkijonosta useampi substringi
merkkijono = "Python-ohjelmointi on hauskaa"
substringi1 = merkkijono[0:6] # Ekstrahoidaan "Python"
substringi2 = merkkijono[17:] # Ekstrahoidaan "hauskaa"
print(substringi1, substringi2) # Tulostaa "Python hauskaa"
``` 

**Huom!** Merkkijonon indeksit alkavat aina nollasta.

## Syvällisempi tarkastelu

Merkkijonojen indeksointi ja substringien ekstrahointi liittyvät tiiviisti toisiinsa Pythonissa. Merkkijonon jokainen merkki on indeksoitu ja niitä voidaan käyttää indekseinä substringien ekstrahoinnissa. Pythonissa on myös monia sisäänrakennettuja metodieja, kuten `find()` ja `split()`, jotka helpottavat substringien löytämistä ja käsittelyä.

## Katso myös

- [Pythonin virallinen dokumentaatio](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
- [GeeksforGeeks - Substring extraction in Python](https://www.geeksforgeeks.org/python-get-a-substring-of-a-given-string/#:~:text=In%20Python%2C%20a%20substring%20is,length%20in%20the%20given%20string.)
- [W3Schools - Python Strings](https://www.w3schools.com/python/python_strings.asp)