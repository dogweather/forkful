---
changelog:
- 2024-02-22, dogweather, reviewed
- 2024-02-22, gpt-4-0125-preview, translated from English
date: 2024-02-22 17:30:17.778833-07:00
description: "Att starta ett nytt projekt i Python handlar om att fr\xE5n b\xF6rjan\
  \ s\xE4tta upp en strukturerad, underh\xE5llbar ram. Programmerare g\xF6r detta\
  \ f\xF6r att s\xE4kerst\xE4lla\u2026"
lastmod: '2024-03-13T22:44:37.483757-06:00'
model: gpt-4-0125-preview
summary: "Att starta ett nytt projekt i Python handlar om att fr\xE5n b\xF6rjan s\xE4\
  tta upp en strukturerad, underh\xE5llbar ram."
title: Att starta ett nytt projekt
weight: 1
---

## Vad & Varför?

Att starta ett nytt projekt i Python handlar om att från början sätta upp en strukturerad, underhållbar ram. Programmerare gör detta för att säkerställa att deras kod är lätt att läsa, felsöka och samarbeta kring, särskilt eftersom projektet och teamet som arbetar med det växer över tid.

## Hur man gör:

### Skapa en virtuell miljö
En virtuell miljö är en självständig katalog som innehåller alla nödvändiga exekverbara filer för att använda de paket som ett Python-projekt skulle behöva. Det är rådligt att skapa en virtuell miljö för varje projekt för att undvika konflikter mellan projektberoenden. Använd `venv`-modulen, som är en del av Pythons standardbibliotek.

```shell
# Byt ut 'myproject' mot namnet på ditt projekt
python3 -m venv myproject-env
```

För att aktivera den virtuella miljön:

På Windows:
```shell
myproject-env\Scripts\activate.bat
```

På Unix eller MacOS:
```shell
source myproject-env/bin/activate
```

Exempel på utdata (utdatan kan variera något beroende på OS):
```shell
(myproject-env) $
```

### Installera paket
Använd `pip`, paketinstallationsprogrammet för Python, för att installera, uppgradera och ta bort paket. Så här kan du installera ett populärt tredjepartsbibliotek, `requests`, för att göra HTTP-förfrågningar:

```shell
pip install requests
```

Exempel på utdata:
```shell
Collecting requests
  Downloading requests-2.25.1-py2.py3-none-any.whl (61 kB)
     |████████████████████████████████| 61 kB 1.3 MB/s
Installerar insamlade paket: requests
Lyckades installera requests-2.25.1
```

### Sätta upp en projektstruktur
Ett typiskt Python-projekt kan se ut ungefär så här:

```
myproject/
│
├── myproject-env/    # Virtuell miljö
├── docs/             # Dokumentation
├── tests/            # Enhetstester och integrationstester
│   └── __init__.py
├── myproject/        # Projektets källkod 
│   ├── __init__.py
│   └── main.py
├── setup.py          # Projektets installationsfil
└── README.md         # Projektöversikt
```

### Skapa ditt första program
Skapa en `main.py`-fil inuti `myproject`-katalogen. Här är ett exempel på ett enkelt program:

```python
# myproject/myproject/main.py
def greet(name):
    return f"Hej, {name}!"

if __name__ == "__main__":
    print(greet("Världen"))
```

Kör ditt program:

```shell
python myproject/main.py
```

Exempel på utdata:
```shell
Hej, Världen!
```

### Använda ett ramverk för större projekt
För större projekt, särskilt webbapplikationer, är ramverk som Django eller Flask ovärderliga. Så här installerar du Flask och skapar en enkel "Hello, World"-webbapplikation:

```shell
pip install Flask
```

Skapa en `app.py`-fil med följande innehåll:

```python
# app.py
from flask import Flask
app = Flask(__name__)

@app.route("/")
def hello_world():
    return "<p>Hej, Världen!</p>"

if __name__ == "__main__":
    app.run(debug=True)
```

Kör Flask-applikationen:

```shell
flask run
```

Exempel på utdata:
```shell
 * Running on http://127.0.0.1:5000/ (Tryck på CTRL+C för att avsluta)
```

Navigera till `http://127.0.0.1:5000/` i din webbläsare, och du bör se meddelandet "Hej, Världen!".
