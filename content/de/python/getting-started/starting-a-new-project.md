---
changelog:
- 2024-02-22, dogweather, reviewed
- 2024-02-22, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-02-22 17:30:12.097340-07:00
description: "Ein neues Projekt in Python zu starten bedeutet, von Anfang an ein strukturiertes,\
  \ wartbares Framework einzurichten. Programmierer machen dies, um\u2026"
lastmod: '2024-02-25T18:49:50.578855-07:00'
model: gpt-4-0125-preview
summary: "Ein neues Projekt in Python zu starten bedeutet, von Anfang an ein strukturiertes,\
  \ wartbares Framework einzurichten. Programmierer machen dies, um\u2026"
title: Ein neues Projekt starten
---

{{< edit_this_page >}}

## Was & Warum?

Ein neues Projekt in Python zu starten bedeutet, von Anfang an ein strukturiertes, wartbares Framework einzurichten. Programmierer machen dies, um sicherzustellen, dass ihr Code leicht zu lesen, zu debuggen und zu bearbeiten ist, besonders wenn das Projekt und das Team, das daran arbeitet, im Laufe der Zeit wachsen.

## Wie man:

### Virtuelle Umgebung erstellen
Eine virtuelle Umgebung ist ein eigenständiges Verzeichnis, das alle notwendigen Ausführbaren enthält, um die Pakete zu verwenden, die ein Python-Projekt benötigen würde. Es ist ratsam, für jedes Projekt eine virtuelle Umgebung zu erstellen, um Konflikte zwischen Projekt-Abhängigkeiten zu vermeiden. Verwende das `venv`-Modul, das Teil der Standardbibliothek von Python ist.

```shell
# Ersetze 'myproject' mit dem Namen deines Projekts
python3 -m venv myproject-env
```

Um die virtuelle Umgebung zu aktivieren:

Unter Windows:
```shell
myproject-env\Scripts\activate.bat
```

Unter Unix oder MacOS:
```shell
source myproject-env/bin/activate
```

Beispielausgabe (die Ausgabe kann je nach Betriebssystem leicht variieren):
```shell
(myproject-env) $
```

### Pakete installieren
Verwende `pip`, den Paketinstaller für Python, um Pakete zu installieren, zu aktualisieren und zu entfernen. Hier ist, wie du eine beliebte Drittanbieter-Bibliothek, `requests`, zum Durchführen von HTTP-Anfragen installieren kannst:

```shell
pip install requests
```

Beispielausgabe:
```shell
Collecting requests
  Downloading requests-2.25.1-py2.py3-none-any.whl (61 kB)
     |████████████████████████████████| 61 kB 1.3 MB/s
Installing collected packages: requests
Erfolgreich installiert requests-2.25.1
```

### Projektstruktur einrichten
Ein typisches Python-Projekt könnte folgendermaßen aussehen:

```
myproject/
│
├── myproject-env/    # Virtuelle Umgebung
├── docs/             # Dokumentation
├── tests/            # Einheiten- und Integrationstests
│   └── __init__.py
├── myproject/        # Quellcode des Projekts
│   ├── __init__.py
│   └── main.py
├── setup.py          # Projektsetup-Datei
└── README.md         # Projektübersicht
```

### Erstelle dein erstes Programm
Erstelle eine `main.py`-Datei im Verzeichnis `myproject`. Hier ist ein Beispiel für ein einfaches Programm:

```python
# myproject/myproject/main.py
def greet(name):
    return f"Hallo, {name}!"

if __name__ == "__main__":
    print(greet("Welt"))
```

Führe dein Programm aus:

```shell
python myproject/main.py
```

Beispielausgabe:
```shell
Hallo, Welt!
```

### Verwende ein Framework für größere Projekte
Für größere Projekte, insbesondere Webanwendungen, sind Frameworks wie Django oder Flask von unschätzbarem Wert. So installierst du Flask und erstellst eine einfache "Hello, World"-Webanwendung:

```shell
pip install Flask
```

Erstelle eine `app.py`-Datei mit folgendem Inhalt:

```python
# app.py
from flask import Flask
app = Flask(__name__)

@app.route("/")
def hello_world():
    return "<p>Hallo, Welt!</p>"

if __name__ == "__main__":
    app.run(debug=True)
```

Führe die Flask-Anwendung aus:

```shell
flask run
```

Beispielausgabe:
```shell
 * Running on http://127.0.0.1:5000/ (Drücke CTRL+C zum Beenden)
```

Navigiere in deinem Webbrowser zu `http://127.0.0.1:5000/`, und du solltest die "Hallo, Welt!"-Nachricht sehen.
