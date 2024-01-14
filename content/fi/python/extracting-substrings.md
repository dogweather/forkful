---
title:    "Python: Alimerkkien erottaminen"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit erottaa osamerkkijonoja Python-ohjelmoinnissa? Substringien erottaminen on hyödyllistä, kun tarvitset tiettyä tietoa merkkijonosta, kuten tietyistä kirjaimista tai numeromerkeistä. Tämä voi auttaa sinua muokkaamaan tai analysoimaan tietoja helpommin.

## Miten

Pythonissa voit erottaa osamerkkijonon käyttämällä hakasulkumerkintää [alku:loppu]. Tämä tarkoittaa, että haluat erottaa merkkijonon tiettyyn kohtaan alkamisesta ja loppuvasta asemaan. Esimerkiksi, jos haluaisit erottaa vain ensimmäiset viisi merkkiä merkkijonosta "Tervetuloa", käyttäisit [0:5]. Alla on esimerkki koodista ja tulosteesta:

```Python
# Määritellään merkkijono
sana = "Tervetuloa"

# Erota osamerkkijono
osamerkki = sana[0:5]

# Tulostus
print(osamerkki)
```

Tämän koodin tuloste olisi "Terve".

## Syvällisempi sukellus

Pythonissa voit myös käyttää negatiivisia indeksejä erottaessasi osamerkkijonoja. Tämä tarkoittaa, että aloitat erottamisen merkkijonon lopusta. Esimerkiksi, jos haluaisit erottaa viimeiset kolme merkkiä merkkijonosta "Tervetuloa", käyttäisit [-3:]. Tässä on esimerkki koodista ja tulosteesta:

```Python
# Määritellään merkkijono
sana = "Tervetuloa"

# Erota osamerkkijono
osamerkki = sana[-3:]

# Tulostus
print(osamerkki)
```

Tämän koodin tuloste olisi "loa".

Voit myös käyttää askelarvoa erottaessasi osamerkkijonoja. Tämä tarkoittaa, että voit määrittää, kuinka monta merkkiä haluat ohittaa erottaessasi. Esimerkiksi, jos haluaisit erottaa joka toisen merkin merkkijonosta "Tervetuloa", käyttäisit [::2]. Tässä on esimerkki koodista ja tulosteesta:

```Python
# Määritellään merkkijono
sana = "Tervetuloa"

# Erota osamerkkijono
osamerkki = sana[::2]

# Tulostus
print(osamerkki)
```

Tämän koodin tuloste olisi "Trvtua".

Erottamista voi myös soveltaa muihin merkkijonon operaatioihin, kuten yhteenlaskuun ja kertolaskuun. Voit myös käyttää sisäkkäisiä hakasulkumerkintöjä erottaaksesi monimutkaisempia osamerkkijonoja.

## Katso myös

- [Pythonin merkkijonot](https://www.w3schools.com/python/python_strings.asp)
- [Oficiallilä Python dokumentaatio substringeista](https://docs.python.org/3/library/stdtypes.html#string-methods)