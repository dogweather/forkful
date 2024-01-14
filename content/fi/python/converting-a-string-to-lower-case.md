---
title:    "Python: Merkkijonon muuttaminen pieniksi kirjaimiksi"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Miksi

Monissa tilanteissa voi olla hyödyllistä muuttaa merkkijono pieniksi kirjaimiksi. Esimerkiksi vertailu ja lajittelemisen helpottamiseksi. Tässä blogikirjoituksessa käsitellään kuinka muuttaa merkkijono pieniksi kirjaimiksi Pythonilla.

## Kuinka

```Python
sana = "TIRKELI"
print(sana.lower())

# Output: tirkeli
```

Pieniksi kirjaimiksi muuttaminen on yksinkertaista Pythonilla! Käytämme sana-muuttujaa, joka sisältää merkkijonon "TIRKELI". Sitten käytämme lower() -metodia muuttaaksemme kaikki kirjaimet pieniksi. Lopuksi tulostamme uuden merkkijonon. Huomaa, että alkuperäinen merkkijono pysyy muuttumattomana, sillä lower() -metodi palauttaa uuden merkkijonon.

```Python
lause = "Opiskelen ohjelmointia"
lause_pienilla = lause.lower()
print(lause)
print(lause_pienilla)

# Output: Opiskelen ohjelmointia
# opiskelen ohjelmointia
```

Voimme myös tallentaa muutetun merkkijonon uuteen muuttujaan, kuten tässä esimerkissä. Tämä on hyödyllistä, jos haluamme käyttää sekä alkuperäistä että muutettua merkkijonoa myöhemmin.

## Syventävä sukellus

Pieniksi kirjaimiksi muuttaminen on vain yksi tapa muokata merkkijonoja Pythonilla. On myös muita hyödyllisiä metodeja, kuten upper() ja capitalize(), jotka muuttavat kirjaimet isoiksi tai tekevät ensimmäisestä kirjaimesta ison. Merkkijonoja voi myös yhdistellä ja jakaa erilaisilla tavoilla.

Pythonilla on myös erilaisia muotoilumahdollisuuksia, kuten muotoilumerkkejä ja tekstitiedoston käyttöä, jotka voivat tehdä merkkijonojen käsittelystä joustavampaa ja tehokkaampaa.

## Katso myös

- [Pythonin merkkijonot](https://www.w3schools.com/python/python_strings.asp)
- [Merkkijonometodien opas](https://www.python-kurs.eu/python3_string_methods.php)
- [Python-tutoriaali](https://www.learnpython.org/en/Strings)