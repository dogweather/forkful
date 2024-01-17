---
title:                "Kommenttiriviparametrien lukeminen"
html_title:           "Python: Kommenttiriviparametrien lukeminen"
simple_title:         "Kommenttiriviparametrien lukeminen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?
Komentoriviparametrien lukeminen tarkoittaa ohjelman antamien argumenttien vastaanottamista käyttäjältä komentokehotteessa. Tätä tehdään yleensä siksi, että ohjelman suorituksessa voidaan käyttää erilaisia parametreja, kuten tiedostonimiä tai vaihtoehtoisia toimintoja.

# Kuinka:
Tässä esimerkeissä osoitetaan, kuinka Pythonin ```argv``` -moduulia käytetään komentoriviparametrien lukemiseen. 

```python
import sys

# Hae argumentit ja tallenna ne listaan
args = sys.argv

# Tulosta kaikki argumentit
print("Saadut argumentit:", args)

# Tulosta ensimmäinen argumentti
print("Ensimmäinen argumentti:", args[0])

# Tulosta toinen argumentti
print("Toinen argumentti:", args[1])
```

Kun koodi suoritetaan komentokehotteessa seuraavasti:

```
python arguments.py arg1 arg2
```

Saadaan seuraava tulos:

```
Saadut argumentit: ['arguments.py', 'arg1', 'arg2']
Ensimmäinen argumentti: arguments.py
Toinen argumentti: arg1
```

# Syväsukellus:
Komentoriviparametrien lukemisen historia juontaa juurensa vanhoista käyttöjärjestelmistä, joissa ohjelmia suoritettiin komentokehotteessa paljon nykyistä enemmän. Nykyään tätä tekniikkaa käytetään edelleen, koska se mahdollistaa ohjelman toiminnan varaamalla tilaa komentokehotteen ulkopuolella. Vaihtoehtoinen tapa lukea komentoriviparametreja Pythonissa on käyttää ```argparse``` -moduulia, joka tarjoaa enemmän mahdollisuuksia argumenttien hallintaan. Argumenttien lukeminen on myös tärkeä taito monissa muissa ohjelmointikielissä, joten sen opetteleminen hyödyttää laajemmin.

# Katso myös:
- [Pythonin sys.argv-dokumentaatio](https://docs.python.org/3/library/sys.html#sys.argv)
- [Pythonin argparse-dokumentaatio](https://docs.python.org/3/library/argparse.html)
- [Komentoriviparametrien lukeminen muilla ohjelmointikielillä](https://en.wikipedia.org/wiki/Command-line_interface#Arguments)