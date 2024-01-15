---
title:                "Muuntaminen merkkijonoksi pienaakkosiksi"
html_title:           "Python: Muuntaminen merkkijonoksi pienaakkosiksi"
simple_title:         "Muuntaminen merkkijonoksi pienaakkosiksi"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi muuttaa merkkijonon pieniksi kirjaimiksi?

Kun työskentelet tekstipohjaisessa ohjelmoinnissa, sinun täytyy usein hallita merkkijonoja. Merkkijonon muuntaminen pieniksi kirjaimiksi voi olla hyödyllistä esimerkiksi silloin, kun haluat verrata kahta tekstiä ja haluat varmistaa, että molemmat ovat samassa muodossa. Tämä voi myös auttaa välttämään virheitä ohjelmassa, jossa tiettyjä merkkijonoja oletetaan olevan tietyssä muodossa.

## Miten se tehdään

```Python
# Alkuperäinen merkkijono
string = "Tämä On Esimerkki"

# Muunna pieniksi kirjaimiksi ja tulosta
print(string.lower())

# Output
tämä on esimerkki
```

Voit muuttaa merkkijonon pieniksi kirjaimiksi käyttämällä `.lower()` -metodia merkkijonolle. Tämä metodi muuttaa kaikki merkkijonon kirjaimet pieniksi kirjaimiksi ja palauttaa uuden merkkijonon. Voit myös tallentaa muunnetun merkkijonon uuteen muuttujaan tai käyttää sitä suoraan tulostamiseen.

```Python
# Esimerkki tallennetusta muunnetusta merkkijonosta
lower_case = string.lower()
print(lower_case)

# Output
tämä on esimerkki
```

## Syväsukellus

Merkkijonon muuttaminen pieniksi kirjaimiksi perustuu siihen, että jokaisella merkillä on tietty numerokoodeihin perustuva esitysmuoto. Pieniksi kirjaimiksi muuttaminen tapahtuu muuttamalla jokaisen merkin numerokoodia vastaavaan pienempään arvoon. Esimerkiksi iso A-kirjain, jonka numerokoodi on 65, muuttuu pieneksi a-kirjaimeksi, jonka numerokoodi on 97.

Pieniksi muuntaminen voi myös aiheuttaa ongelmia, jos merkkijonossa on muita kuin kirjaimia, kuten välimerkkejä tai numeroita. Voit kuitenkin käyttää muita metodeja, kuten `.strip()` poistamaan ylimääräiset merkit ennen muuntamista.

## Katso myös

- [Pythonin virallinen dokumentaatio merkkijonojen käsittelystä](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Merkkijonon muuttaminen isoiksi kirjaimiksi](https://pynative.com/python-string-to-upper-case/)
- [Tabulaattoreiden ja rivinvaihtojen poistaminen merkkijonoista](https://www.geeksforgeeks.org/python-string-strip-method/)