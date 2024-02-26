---
changelog:
- 2024-02-22, dogweather, reviewed
- 2024-02-22, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-02-22 17:29:57.476585-07:00
description: "\xC5 starte et nytt prosjekt i Python handler om \xE5 sette opp et strukturert,\
  \ vedlikeholdbart rammeverk fra starten. Programmerere gj\xF8r dette for \xE5 sikre\
  \ at\u2026"
lastmod: 2024-02-25 18:26:58.230785
model: gpt-4-0125-preview
summary: "\xC5 starte et nytt prosjekt i Python handler om \xE5 sette opp et strukturert,\
  \ vedlikeholdbart rammeverk fra starten. Programmerere gj\xF8r dette for \xE5 sikre\
  \ at\u2026"
title: Starter et nytt prosjekt
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å starte et nytt prosjekt i Python handler om å sette opp et strukturert, vedlikeholdbart rammeverk fra starten. Programmerere gjør dette for å sikre at koden deres er lett å lese, feilsøke og samarbeide om, spesielt ettersom prosjektet og teamet som jobber med det vokser over tid.

## Hvordan:

### Opprette et virtuelt miljø
Et virtuelt miljø er en selvstendig mappe som inneholder alle de nødvendige kjørbare filene for å bruke pakkene som et Python-prosjekt trenger. Det anbefales å opprette et virtuelt miljø for hvert prosjekt for å unngå konflikter mellom prosjektavhengigheter. Bruk `venv`-modulen, som er en del av det standard Python-biblioteket.

```shell
# Erstatt 'myproject' med navnet på prosjektet ditt
python3 -m venv myproject-env
```

For å aktivere det virtuelle miljøet:

På Windows:
```shell
myproject-env\Scripts\activate.bat
```

På Unix eller MacOS:
```shell
source myproject-env/bin/activate
```

Eksempel på utdata (utdata kan variere litt avhengig av operativsystemet):
```shell
(myproject-env) $
```

### Installere pakker
Bruk `pip`, installeringsprogrammet for Python-pakker, for å installere, oppgradere og fjerne pakker. Slik kan du installere et populært tredjepartsbibliotek, `requests`, for å gjøre HTTP-forespørsler:

```shell
pip install requests
```

Eksempel på utdata:
```shell
Collecting requests
  Downloading requests-2.25.1-py2.py3-none-any.whl (61 kB)
     |████████████████████████████████| 61 kB 1.3 MB/s
Installing collected packages: requests
Successfully installed requests-2.25.1
```

### Sette opp en prosjektstruktur
Et typisk Python-prosjekt kan se noe slik ut:

```
myproject/
│
├── myproject-env/    # Virtuelt miljø
├── docs/             # Dokumentasjon
├── tests/            # Enhet- og integrasjonstester
│   └── __init__.py
├── myproject/        # Prosjektkildekode 
│   ├── __init__.py
│   └── main.py
├── setup.py          # Prosjektoppsettsfil
└── README.md         # Prosjektoversikt
```

### Lag ditt første program
Lag en `main.py`-fil inne i `myproject`-mappen. Her er et eksempel på et enkelt program:

```python
# myproject/myproject/main.py
def greet(name):
    return f"Hei, {name}!"

if __name__ == "__main__":
    print(greet("Verden"))
```

Kjør programmet ditt:

```shell
python myproject/main.py
```

Eksempel på utdata:
```shell
Hei, Verden!
```

### Bruk et rammeverk for større prosjekter
For større prosjekter, spesielt webapplikasjoner, er rammeverk som Django eller Flask uvurderlige. Slik installerer du Flask og oppretter en enkel "Hei, Verden"-webapplikasjon:

```shell
pip install Flask
```

Lag en `app.py`-fil med følgende innhold:

```python
# app.py
from flask import Flask
app = Flask(__name__)

@app.route("/")
def hello_world():
    return "<p>Hei, Verden!</p>"

if __name__ == "__main__":
    app.run(debug=True)
```

Kjør Flask-applikasjonen:

```shell
flask run
```

Eksempel på utdata:
```shell
 * Running on http://127.0.0.1:5000/ (Trykk CTRL+C for å avslutte)
```

Naviger til `http://127.0.0.1:5000/` i nettleseren din, og du bør se "Hei, Verden!"-meldingen.
