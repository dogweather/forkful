---
changelog:
- 2024-02-22, dogweather, reviewed
- 2024-02-22, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-02-22 17:30:29.394060-07:00
description: "Uuden projektin aloittaminen Pythonilla tarkoittaa rakenteellisen, yll\xE4\
  pidett\xE4v\xE4n kehyksen pystytt\xE4mist\xE4 alusta alkaen. Ohjelmoijat tekev\xE4\
  t n\xE4in\u2026"
lastmod: 2024-02-25 18:27:12.755496
model: gpt-4-0125-preview
summary: "Uuden projektin aloittaminen Pythonilla tarkoittaa rakenteellisen, yll\xE4\
  pidett\xE4v\xE4n kehyksen pystytt\xE4mist\xE4 alusta alkaen. Ohjelmoijat tekev\xE4\
  t n\xE4in\u2026"
title: Uuden projektin aloittaminen
---

{{< edit_this_page >}}

## Mikä & Miksi?

Uuden projektin aloittaminen Pythonilla tarkoittaa rakenteellisen, ylläpidettävän kehyksen pystyttämistä alusta alkaen. Ohjelmoijat tekevät näin varmistaakseen, että heidän koodinsa on helppolukuista, debugattavaa ja yhteistyöhön sopivaa, erityisesti kun projekti ja sen parissa työskentelevä tiimi kasvavat ajan myötä.

## Kuinka:

### Luo virtuaaliympäristö
Virtuaaliympäristö on itsenäinen hakemisto, joka sisältää kaikki tarvittavat suoritettavat tiedostot paketteja varten, joita Python-projekti saattaa tarvita. On suositeltavaa luoda virtuaaliympäristö jokaiselle projektille välttääkseen riippuvuuksien väliset konfliktit. Käytä `venv`-moduulia, joka on osa Pythonin standardikirjastoa.

```shell
# Korvaa 'myproject' projektisi nimellä
python3 -m venv myproject-env
```

Virtuaaliympäristön aktivoiminen:

Windowsissa:
```shell
myproject-env\Scripts\activate.bat
```

Unixissa tai MacOS:ssa:
```shell
source myproject-env/bin/activate
```

Esimerkkitulo (tulo voi hieman vaihdella käyttöjärjestelmän mukaan):
```shell
(myproject-env) $
```

### Pakettien asentaminen
Käytä `pip`-ohjelmaa, Pythonin paketin asentajaa, pakettien asentamiseen, päivittämiseen ja poistamiseen. Näin voit asentaa suositun kolmannen osapuolen kirjaston, `requests`, tehdäksesi HTTP-pyyntöjä:

```shell
pip install requests
```

Esimerkkitulo:
```shell
Collecting requests
  Downloading requests-2.25.1-py2.py3-none-any.whl (61 kB)
     |████████████████████████████████| 61 kB 1.3 MB/s
Installing collected packages: requests
Successfully installed requests-2.25.1
```

### Projektirakenteen luominen
Tyypillinen Python-projekti saattaa näyttää jotakuinkin tältä:

```
myproject/
│
├── myproject-env/    # Virtuaaliympäristö
├── docs/             # Dokumentaatio
├── tests/            # Yksikkö- ja integraatiotestit
│   └── __init__.py
├── myproject/        # Projektin lähdekoodi
│   ├── __init__.py
│   └── main.py
├── setup.py          # Projekti asetustiedosto
└── README.md         # Projektin yleiskatsaus
```

### Luo ensimmäinen ohjelmasi
Luo `main.py`-tiedosto `myproject`-hakemistoon. Tässä on esimerkki yksinkertaisesta ohjelmasta:

```python
# myproject/myproject/main.py
def greet(name):
    return f"Hei, {name}!"

if __name__ == "__main__":
    print(greet("Maailma"))
```

Suorita ohjelmasi:

```shell
python myproject/main.py
```

Esimerkkitulo:
```shell
Hei, Maailma!
```

### Käytä runkoa suuremmissa projekteissa
Suuremmissa projekteissa, erityisesti web-sovelluksissa, rungot kuten Django tai Flask ovat korvaamattomia. Näin voit asentaa Flaskin ja luoda yksinkertaisen "Hello, World" -web-sovelluksen:

```shell
pip install Flask
```

Luo tiedosto `app.py` seuraavalla sisällöllä:

```python
# app.py
from flask import Flask
app = Flask(__name__)

@app.route("/")
def hello_world():
    return "<p>Hei, Maailma!</p>"

if __name__ == "__main__":
    app.run(debug=True)
```

Suorita Flask-sovellus:

```shell
flask run
```

Esimerkkitulo:
```shell
 * Running on http://127.0.0.1:5000/ (Paina CTRL+C lopettaaksesi)
```

Siirry web-selaimellasi osoitteeseen `http://127.0.0.1:5000/`, ja sinun pitäisi nähdä "Hei, Maailma!" -viesti.
