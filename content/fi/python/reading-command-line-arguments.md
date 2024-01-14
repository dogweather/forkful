---
title:    "Python: Komentoriviparametrien lukeminen"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Monet ohjelmat ovat ohjelmoitu ottamaan vastaan käyttäjän antamia argumentteja komentokehotteessa. Näitä argumentteja voidaan käyttää ohjelman toiminnan määrittämiseen ja sen avulla voidaan suorittaa erilaisia ohjelmointityökaluja. On tärkeää ymmärtää, miten lukea nämä komentorivin argumentit, jotta voit käyttää ohjelmia tehokkaasti.

## Miten

Lue komentorivin argumentit Pythonissa käyttämällä `sys.argv` -moduulia. Tällä moduulilla voi käsitellä komentokehotteessa annetut argumentit ja käyttää niitä ohjelman suorituksessa.

```Python
import sys

# Tulostaa kaikki annetut argumentit
for arg in sys.argv:
    print(arg)
```

Esimerkki antaa argumentteja käyttämällä `python` -komentoa komentokehotteessa. Käytä `python filename.py argument1 argument2 ...` komennota antaaksesi ohjelmalle argumentteja. Ohjelma tulostaa kaikki annetut argumentit.

```
python example.py Hello World!
```

Output:

```
example.py
Hello
World
```

## Syvällisempi Tutustuminen

`sys.argv` -moduuli voidaan myös yhdistää muihin Pythonin moduuleihin, kuten `argparse` -moduuliin, joka antaa käyttäjän määrittää argumenttien tyypin ja käyttää niitä tiettyn tavalla.

```Python
import argparse

# Luo argparse -parseri
parser = argparse.ArgumentParser(description='Laskee summan kahdelle numeroille.')

# Lisää tarvittavat argumentit parseriin
parser.add_argument('number1', type=int, help='Ensimmäinen numero')
parser.add_argument('number2', type=int, help='Toinen numero')

# Luo args -muuttuja, johon tallennetaan argumentit
args = parser.parse_args()

# Tulosta argumenttien antamien arvojen summa
print(args.number1 + args.number2)
```

Tämä esimerkki ottaa vastaan kaksi numeroa komentorivolta ja laskee niiden summan. Argumenttien antamiseen tarvitaan `python` -komennon lisäksi myös ohjelman nimi sekä argumentit kuten `python example.py 5 10`.

Output:

```
15
```

## Katso Myös

- [Python `sys` -moduulin dokumentaatio](https://docs.python.org/3/library/sys.html)
- [Python `argparse` -moduulin dokumentaatio](https://docs.python.org/3/library/argparse.html)