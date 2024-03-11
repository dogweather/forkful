---
changelog:
- 2024-02-22, dogweather, reviewed
- 2024-02-22, gpt-4-0125-preview, translated from English
date: 2024-02-22 17:31:02.639168-07:00
description: "Avviare un nuovo progetto in Python significa impostare sin dall'inizio\
  \ una struttura organizzata e mantenibile. I programmatori fanno ci\xF2 per garantire\u2026"
lastmod: '2024-03-11T00:14:16.556067-06:00'
model: gpt-4-0125-preview
summary: "Avviare un nuovo progetto in Python significa impostare sin dall'inizio\
  \ una struttura organizzata e mantenibile. I programmatori fanno ci\xF2 per garantire\u2026"
title: Iniziare un nuovo progetto
---

{{< edit_this_page >}}

## Cosa & Perché?

Avviare un nuovo progetto in Python significa impostare sin dall'inizio una struttura organizzata e mantenibile. I programmatori fanno ciò per garantire che il loro codice sia facile da leggere, da debuggare e su cui collaborare, specialmente man mano che il progetto e il team che ci lavora crescono nel tempo.

## Come fare:

### Creare un Ambiente Virtuale
Un ambiente virtuale è una directory autonoma che contiene tutti gli eseguibili necessari per usare i pacchetti di cui un progetto Python potrebbe aver bisogno. È consigliabile creare un ambiente virtuale per ogni progetto per evitare conflitti tra le dipendenze dei progetti. Usare il modulo `venv`, che fa parte della libreria standard di Python.

```shell
# Sostituire 'myproject' con il nome del proprio progetto
python3 -m venv myproject-env
```

Per attivare l'ambiente virtuale:

Su Windows:
```shell
myproject-env\Scripts\activate.bat
```

Su Unix o MacOS:
```shell
source myproject-env/bin/activate
```

Output di esempio (l'output può variare leggermente a seconda del sistema operativo):
```shell
(myproject-env) $
```

### Installare Pacchetti
Usare `pip`, l'installer di pacchetti per Python, per installare, aggiornare e rimuovere pacchetti. Ecco come si può installare una popolare libreria di terze parti, `requests`, per effettuare richieste HTTP:

```shell
pip install requests
```

Output di esempio:
```shell
Collecting requests
  Downloading requests-2.25.1-py2.py3-none-any.whl (61 kB)
     |████████████████████████████████| 61 kB 1.3 MB/s
Installing collected packages: requests
Successfully installed requests-2.25.1
```

### Impostare una Struttura di Progetto
Un tipico progetto Python potrebbe avere un aspetto simile a questo:

```
myproject/
│
├── myproject-env/    # Ambiente virtuale
├── docs/             # Documentazione
├── tests/            # Test unitari e di integrazione
│   └── __init__.py
├── myproject/        # Codice sorgente del progetto
│   ├── __init__.py
│   └── main.py
├── setup.py          # File di impostazione del progetto
└── README.md         # Panoramica del progetto
```

### Creare il Primo Programma
Creare un file `main.py` all'interno della directory `myproject`. Ecco un esempio di un programma semplice:

```python
# myproject/myproject/main.py
def greet(name):
    return f"Ciao, {name}!"

if __name__ == "__main__":
    print(greet("Mondo"))
```

Eseguire il programma:

```shell
python myproject/main.py
```

Output di esempio:
```shell
Ciao, Mondo!
```

### Usare un Framework per Progetti Più Grandi
Per progetti più grandi, specialmente applicazioni web, framework come Django o Flask sono inestimabili. Ecco come installare Flask e creare una semplice applicazione web "Hello, World":

```shell
pip install Flask
```

Creare un file `app.py` con il seguente contenuto:

```python
# app.py
from flask import Flask
app = Flask(__name__)

@app.route("/")
def hello_world():
    return "<p>Ciao, Mondo!</p>"

if __name__ == "__main__":
    app.run(debug=True)
```

Eseguire l'applicazione Flask:

```shell
flask run
```

Output di esempio:
```shell
 * Running on http://127.0.0.1:5000/ (Premi CTRL+C per interrompere)
```

Navigare su `http://127.0.0.1:5000/` nel proprio browser web e si dovrebbe vedere il messaggio "Ciao, Mondo!".
