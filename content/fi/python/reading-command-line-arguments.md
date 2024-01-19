---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Elm: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Mikä & Miksi?

Komentoriviparametrit ovat ohjelmistoille annettuja syötteitä, jotka antavat lisäohjeita ohjelman suoritukseen. Ohjelmoijat käyttävät niitä ohjaamaan ohjelmien toimintaa ilman koodin muokkaamista.

# Näin teet:

Pythonissa `sys`-moduulin `argv`-lista sisältää komentorivilta annetut argumentit. Argumentit luetaan ohjelman käynnistyessä.

```Python
import sys

print("Komentoriviparametrit:")

for i in sys.argv:
    print(i)
```

Jos ajat yllä olevan koodin komentoriviltä antamalla lisäargumentteja (esim. `python koodi.py Hei Maailma`), tuloste näkyy seuraavasti:

```Shell
Komentoriviparametrit:
koodi.py
Hei
Maailma
```

# Syvempi syvennys:

1. Historiallinen yhteys: Komentoriviparametrinluku on peräisin Unix-käyttöjärjestelmän ajoilta ja on edelleen hyödyllinen ohjelmoijille.

2. Vaihtoehdot: Voit käyttää myös `argparse`-moduulia monimutkaisempien argumenttivaatimusten mukaan.

3. Toteutuksen yksityiskohdat: Huomaa, että `sys.argv` sisältää ensin skriptin nimen (tässä tapauksessa "koodi.py") ja sitten komentorivilta saadut argumentit.

# Lisätietolähteitä:

1. Pythonin virallinen dokumentaatio: [sys.argv](https://docs.python.org/3/library/sys.html)
2. Pythonin virallinen dokumentaatio: [argparse](https://docs.python.org/3/library/argparse.html) 
3. Blogikirjoitus: [Command Line arguments in Python](https://realpython.com/pyscripter-command-line-arguments/)