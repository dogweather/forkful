---
changelog:
- 2024-02-22, dogweather, reviewed
- 2024-02-22, gpt-4-0125-preview, translated from English
date: 2024-02-22 17:30:10.136520-07:00
description: "Rozpoczynaj\u0105c nowy projekt w Pythonie chodzi o to, aby od samego\
  \ pocz\u0105tku ustawi\u0107 zorganizowan\u0105 i \u0142atw\u0105 w utrzymaniu struktur\u0119\
  . Programi\u015Bci robi\u0105 to, aby\u2026"
lastmod: '2024-03-13T22:44:34.950070-06:00'
model: gpt-4-0125-preview
summary: "Rozpoczynaj\u0105c nowy projekt w Pythonie chodzi o to, aby od samego pocz\u0105\
  tku ustawi\u0107 zorganizowan\u0105 i \u0142atw\u0105 w utrzymaniu struktur\u0119\
  ."
title: Rozpoczynanie nowego projektu
weight: 1
---

## Co i dlaczego?

Rozpoczynając nowy projekt w Pythonie chodzi o to, aby od samego początku ustawić zorganizowaną i łatwą w utrzymaniu strukturę. Programiści robią to, aby zapewnić, że ich kod jest łatwy do odczytania, debugowania i współpracy, szczególnie w miarę rozwoju projektu i zespołu pracującego nad nim w czasie.

## Jak to zrobić:

### Utwórz środowisko wirtualne
Środowisko wirtualne to samodzielny katalog, który zawiera wszystkie niezbędne pliki wykonywalne do użytkowania pakietów, których projekt Pythona może potrzebować. Zaleca się tworzenie oddzielnego środowiska wirtualnego dla każdego projektu, aby uniknąć konfliktów między zależnościami projektu. Użyj modułu `venv`, który jest częścią standardowej biblioteki Pythona.

```shell
# Zastąp 'myproject' nazwą swojego projektu
python3 -m venv myproject-env
```

Aby aktywować środowisko wirtualne:

W Windows:
```shell
myproject-env\Scripts\activate.bat
```

W Unix lub MacOS:
```shell
source myproject-env/bin/activate
```

Przykładowy wynik (wynik może się nieznacznie różnić w zależności od systemu operacyjnego):
```shell
(myproject-env) $
```

### Instalowanie pakietów
Użyj `pip`, instalatora pakietów dla Pythona, aby instalować, aktualizować i usuwać pakiety. Oto jak możesz zainstalować popularną bibliotekę stron trzecich, `requests`, do wykonywania żądań HTTP:

```shell
pip install requests
```

Przykładowy wynik:
```shell
Collecting requests
  Downloading requests-2.25.1-py2.py3-none-any.whl (61 kB)
     |████████████████████████████████| 61 kB 1.3 MB/s
Installing collected packages: requests
Successfully installed requests-2.25.1
```

### Ustawianie struktury projektu
Typowy projekt Pythona może wyglądać mniej więcej tak:

```
myproject/
│
├── myproject-env/    # Środowisko wirtualne
├── docs/             # Dokumentacja
├── tests/            # Testy jednostkowe i integracyjne
│   └── __init__.py
├── myproject/        # Kod źródłowy projektu 
│   ├── __init__.py
│   └── main.py
├── setup.py          # Plik ustawień projektu
└── README.md         # Przegląd projektu
```

### Utwórz swój pierwszy program
Utwórz plik `main.py` w katalogu `myproject`. Oto przykład prostego programu:

```python
# myproject/myproject/main.py
def greet(name):
    return f"Witaj, {name}!"

if __name__ == "__main__":
    print(greet("świecie"))
```

Uruchom swój program:

```shell
python myproject/main.py
```

Przykładowy wynik:
```shell
Witaj, świecie!
```

### Użyj frameworka dla większych projektów
W przypadku większych projektów, szczególnie aplikacji internetowych, frameworki takie jak Django czy Flask są nieocenione. Oto jak zainstalować Flask i stworzyć prostą aplikację internetową "Hello, World":

```shell
pip install Flask
```

Utwórz plik `app.py` z następującą zawartością:

```python
# app.py
from flask import Flask
app = Flask(__name__)

@app.route("/")
def hello_world():
    return "<p>Witaj, świecie!</p>"

if __name__ == "__main__":
    app.run(debug=True)
```

Uruchom aplikację Flask:

```shell
flask run
```

Przykładowy wynik:
```shell
 * Running on http://127.0.0.1:5000/ (Press CTRL+C to quit)
```

Przejdź do `http://127.0.0.1:5000/` w przeglądarce internetowej, a powinieneś zobaczyć wiadomość "Witaj, świecie!".
