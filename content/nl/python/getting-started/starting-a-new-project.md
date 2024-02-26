---
changelog:
- 2024-02-22, dogweather, reviewed
- 2024-02-22, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-02-22 17:30:11.656533-07:00
description: "Een nieuw project starten in Python gaat over het opzetten van een gestructureerd,\
  \ onderhoudbaar kader vanaf het begin. Programmeurs doen dit om ervoor te\u2026"
lastmod: '2024-02-25T18:49:47.771274-07:00'
model: gpt-4-0125-preview
summary: "Een nieuw project starten in Python gaat over het opzetten van een gestructureerd,\
  \ onderhoudbaar kader vanaf het begin. Programmeurs doen dit om ervoor te\u2026"
title: Een nieuw project starten
---

{{< edit_this_page >}}

## Wat & Waarom?

Een nieuw project starten in Python gaat over het opzetten van een gestructureerd, onderhoudbaar kader vanaf het begin. Programmeurs doen dit om ervoor te zorgen dat hun code gemakkelijk te lezen, te debuggen en samen aan te werken is, vooral naarmate het project en het team dat eraan werkt in de loop van de tijd groeien.

## Hoe te:

### Maak een Virtuele Omgeving
Een virtuele omgeving is een zelfstandige map die alle benodigde uitvoerbare bestanden bevat om de pakketten te gebruiken die een Python-project nodig zou hebben. Het is raadzaam om voor elk project een virtuele omgeving te creëren om conflicten tussen projectafhankelijkheden te vermijden. Gebruik de `venv`-module, die deel uitmaakt van de standaard Python-bibliotheek.

```shell
# Vervang 'mijnproject' met de naam van jouw project
python3 -m venv mijnproject-env
```

Om de virtuele omgeving te activeren:

Op Windows:
```shell
mijnproject-env\Scripts\activate.bat
```

Op Unix of MacOS:
```shell
source mijnproject-env/bin/activate
```

Voorbeelduitvoer (de uitvoer kan enigszins variëren afhankelijk van het besturingssysteem):
```shell
(mijnproject-env) $
```

### Pakketten Installeren
Gebruik `pip`, de pakketinstallateur voor Python, om pakketten te installeren, upgraden en verwijderen. Hier is hoe je een populaire bibliotheek van derden, `requests`, kunt installeren om HTTP-verzoeken te maken:

```shell
pip install requests
```

Voorbeelduitvoer:
```shell
Collecting requests
  Downloading requests-2.25.1-py2.py3-none-any.whl (61 kB)
     |████████████████████████████████| 61 kB 1.3 MB/s
Installing collected packages: requests
Successfully installed requests-2.25.1
```

### Een Projectstructuur Opzetten
Een typisch Python-project kan er ongeveer zo uitzien:

```
mijnproject/
│
├── mijnproject-env/    # Virtuele omgeving
├── docs/               # Documentatie
├── tests/              # Unit- en integratietests
│   └── __init__.py
├── mijnproject/        # Broncode van het project 
│   ├── __init__.py
│   └── main.py
├── setup.py            # Project setup bestand
└── README.md           # Projectoverzicht
```

### Maak Je Eerste Programma
Maak een `main.py` bestand binnen de `mijnproject` map. Hier is een voorbeeld van een eenvoudig programma:

```python
# mijnproject/mijnproject/main.py
def groet(naam):
    return f"Hallo, {naam}!"

if __name__ == "__main__":
    print(groet("Wereld"))
```

Draai je programma:

```shell
python mijnproject/main.py
```

Voorbeelduitvoer:
```shell
Hallo, Wereld!
```

### Gebruik een Framework voor Grotere Projecten
Voor grotere projecten, vooral webapplicaties, zijn frameworks zoals Django of Flask van onschatbare waarde. Hier is hoe je Flask installeert en een eenvoudige "Hallo, Wereld" webapplicatie maakt:

```shell
pip install Flask
```

Maak een `app.py` bestand met de volgende inhoud:

```python
# app.py
from flask import Flask
app = Flask(__name__)

@app.route("/")
def hallo_wereld():
    return "<p>Hallo, Wereld!</p>"

if __name__ == "__main__":
    app.run(debug=True)
```

Draai de Flask-applicatie:

```shell
flask run
```

Voorbeelduitvoer:
```shell
 * Draait op http://127.0.0.1:5000/ (Druk op CTRL+C om te stoppen)
```

Navigeer naar `http://127.0.0.1:5000/` in je webbrowser, en je zou de boodschap "Hallo, Wereld!" moeten zien.
