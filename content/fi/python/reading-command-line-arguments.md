---
title:                "Python: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Komentoriviparametrit ovat tärkeä osa Python-ohjelmointia ja niiden ymmärtäminen voi auttaa sinua tekemään ohjelmistostasi joustavampaa ja monipuolisempaa.

## Miten

Komentoriviparametrit annetaan Python-ohjelmalle, kun sitä suoritetaan terminaalissa. Ne ovat erittäin hyödyllisiä, kun haluat muuttaa ohjelman käyttäytymistä ilman, että joudut muokkaamaan itse ohjelmakoodia.

Esimerkiksi voit luoda ohjelman, joka laskee kahden luvun summan, ja antaa käyttäjän valita kummankin luvun komentoriviparametreilla. Alla on esimerkki koodista ja sen tulostuksesta:

```Python 
import sys

number1 = int(sys.argv[1])
number2 = int(sys.argv[2])

sum = number1 + number2
print("Summa: ", sum)

```

Kun suoritat tämän ohjelman terminaalissa antamalla kaksi lukua, näet napakan summatulosteen.

```
$ python summa.py 5 3
Summa: 8
```

Voit myös antaa ohjelmalle vaihtoehtoja, jotka muuttavat sen toimintaa. Esimerkiksi voit lisätä "-t" vaihtoehdon, joka tulostaa kertotaulun sijasta kertotaulun kertomalla tulon.

```Python
import sys

number1 = int(sys.argv[2])
number2 = int(sys.argv[3])

if sys.argv[1] == "-t":
    product = number1 * number2
else:
    product = number1 * number2
    print("Kertotaulu: ", sys.argv[1], "x", sys.argv[2], "=", product)
```

Tässä esimerkissä käytämme "sys.argv" muuttujaa tarkistamaan, mitä vaihtoehtoa käyttäjä on antanut. Suorittamalla tämän ohjelman seuraavalla tavalla, näet erilaisen tulosteen:

```
$ python kertotaulu.py -t 5 3
Kertolasku: 5 x 3 = 15
```

## Syvempi sukellus

Komentoriviparametrit tarjoavat paljon mahdollisuuksia ja niitä voi käyttää monella eri tavalla ohjelman kehityksessä. Voit esimerkiksi antaa käyttäjälle mahdollisuuden valita tiedosto, johon ohjelman tuloksia tallennetaan, tai lisätä väliaikaisia parametreja, jotka muuttavat ohjelman suoritusta.

Komentoriviparametrit toimivat komentorivin ja ohjelman välillä, joten niitä voidaan käyttää myös Skripteissä, joita suoritetaan käyttöjärjestelmän sisällä.

## Katso myös

- [Pythonin virallinen dokumentaatio komentoriviparametreista] (https://docs.python.org/3/library/sys.html#sys.argv)
- [RealPythonin opas komentoriviparametrien käyttöön] (https://realpython.com/python-command-line-arguments/)
- [Keskustelu Stack Overflowissa komentoriviparametrien käyttöönotosta Pythonissa] (https://stackoverflow.com/questions/11210104/how-can-i-read-command-line-arguments-in-python)