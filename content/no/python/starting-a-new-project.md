---
title:                "Å starte et nytt prosjekt"
date:                  2024-01-20T18:04:19.165200-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å starte et nytt prosjekt"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Å starte et nytt Python-prosjekt er som å legge ut på en reise; du begynner med en tom mappe og ender opp med et kjørbar system som løser et problem. Programmerere skaper nye prosjekter for å teste ideer, løse problemer eller lære noe nytt.

## How to: (Hvordan:)
Opprette et nytt prosjekt:

```Python
# Installer virtualenv for å opprette et isolert Python-miljø
pip install virtualenv

# Opprett en ny mappe for ditt prosjekt
mkdir mitt_prosjekt
cd mitt_prosjekt

# Opprett et virtual environment i prosjektmappen
python -m venv venv

# Aktiver virtual environment (for Windows)
venv\Scripts\activate.bat

# Aktiver virtual environment (for MacOS/Linux)
source venv/bin/activate

# Installer nødvendige pakker med pip
pip install flask

# Lag en enkel Flask-applikasjon
echo "from flask import Flask
app = Flask(__name__)

@app.route('/')
def home():
    return 'Hei Verden!'

if __name__ == '__main__':
    app.run()" > app.py

# Kjør Flask-applikasjonen
python app.py
```

Sample output:

```
 * Running on http://127.0.0.1:5000/ (Press CTRL+C to quit)
```

Åpne en nettleser og gå til http://127.0.0.1:5000/ for å se teksten 'Hei Verden!'.

## Deep Dive (Dypdykk)
Før virtual environments ble standard, jobbet pythonutviklere globalt med én installasjon, noe som skapte konflikter mellom prosjektavhengigheter. Alternativer til `virtualenv` inkluderer `conda`, som håndterer både Python-pakker og binærfiler. For større prosjekter brukes ofte Docker, som pakker hele kjøreomgivelser og avhengigheter i isolerte containere. Bruk av `virtualenv` eller lignende teknologier er essensielt for å unngå "dependency hell".

## See Also (Se Også)
- Det offisielle Python-dokumentasjonen for `virtualenv`: https://docs.python.org/3/library/venv.html
- Flask-dokumentasjon for å komme i gang: https://flask.palletsprojects.com/en/2.0.x/quickstart/
- Docker-dokumentasjon for mer komplekse miljøer: https://docs.docker.com/get-started/
