---
title:                "Uuden projektin aloittaminen"
date:                  2024-01-20T18:04:14.542738-07:00
model:                 gpt-4-1106-preview
simple_title:         "Uuden projektin aloittaminen"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Uuden projektin aloitus on tyhjän koodisivun muuttamista alustaviksi tiedostoiksi ja koodirungoiksi. Koodarit tekevät tämän, jotta saavat puhtaan pohjan, josta kasvattaa ideansa.

## How to: (Kuinka tehdä:)
Python-projektin voi aloittaa luomalla uuden virtuaaliympäristön ja asentamalla tarvittavat paketit. Tässä esimerkki:

```Python
# Asenna virtualenv, jos se ei ole vielä asennettu
pip install virtualenv

# Luo uusi virtuaaliympäristö nimeltä 'projekti'
virtualenv projekti

# Aktivoi virtuaaliympäristö
# Windows:
projekti\Scripts\activate
# Linux tai macOS:
source projekti/bin/activate

# Asenna paketit käyttäen pip-komentoa
pip install flask

# Luo uusi tiedosto 'app.py'
touch app.py  # Linux/macOS tai 'type nul > app.py' Windowsissa

# Kirjoita seuraava Flask-esimerkkisovellus 'app.py'-tiedostoon
import flask

app = flask.Flask(__name__)

@app.route('/')
def home():
    return "Hello, World!"

# Suorita sovellus
export FLASK_APP=app  # Linux/macOS tai 'set FLASK_APP=app' Windowsissa
flask run
```

Kun suoritat viimeisen komennon, saat näkyviin:

```
 * Running on http://127.0.0.1:5000/
```

Avaa selain ja mene osoitteeseen http://127.0.0.1:5000/ nähdäksesi tervehdyksen.

## Deep Dive (Sukellus syvyyksiin)
Projektin aloituksella on pitkä historia. Aikana, jolloin koko koodi kirjoitettiin alusta loppuun joka kerta, projektit aloitettiin tyhjältä pöydältä. Virtuaaliympäristöt ja paketinhallintajärjestelmät, kuten `pip`, tulivat peliin ja ovat nykyisin keskeinen osa Python-koodarien arkea.

On olemassa vaihtoehtoja. Joissakin tapauksissa koodarit voivat käyttää `conda`-ympäristöjä, erityisesti tieteellisessä laskennassa tai jos käytetään erityisiä C-kirjastoja. Työkalut kuten `Poetry` ja `Pipenv` yhdistävät riippuvuuksien hallinnan ja paketinhallinnan, tarjoten tiukan kontrollin yli sovellusriippuvuuksista.

Projektin aloittamisessa tärkeää on johdonmukaisuus ja ylläpidettävyys. Hyvät käytännöt, kuten kommentointi, testaus ja dokumentaatio, kannattaa aloittaa ajoissa.

## See Also (Katso myös)
- Pythonin virallinen dokumentaatio virtuaaliympäristöistä: https://docs.python.org/3/tutorial/venv.html
- Flaskin aloitussivu: http://flask.pocoo.org/
- `pip`-dokumentaatio: https://pip.pypa.io/en/stable/
- `virtualenv` dokumentaatio: https://virtualenv.pypa.io/en/latest/
- `conda` dokumentaatio: https://docs.conda.io/en/latest/
- `Poetry` dokumentaatio: https://python-poetry.org/docs/
- `Pipenv` dokumentaatio: https://pipenv.pypa.io/en/latest/
