---
date: 2024-01-20 18:04:35.132822-07:00
description: "Tworzenie nowego projektu to jak otwieranie pustego zeszytu. Programi\u015B\
  ci robi\u0105 to, by rozwi\u0105zywa\u0107 problemy, uczy\u0107 si\u0119 i tworzy\u0107\
  \ co\u015B nowego."
lastmod: 2024-02-19 22:04:54.134302
model: gpt-4-1106-preview
summary: "Tworzenie nowego projektu to jak otwieranie pustego zeszytu. Programi\u015B\
  ci robi\u0105 to, by rozwi\u0105zywa\u0107 problemy, uczy\u0107 si\u0119 i tworzy\u0107\
  \ co\u015B nowego."
title: Rozpoczynanie nowego projektu
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Tworzenie nowego projektu to jak otwieranie pustego zeszytu. Programiści robią to, by rozwiązywać problemy, uczyć się i tworzyć coś nowego.

## How to: (Jak to zrobić:)
```python
# Instalacja środowiska wirtualnego (virtualenv)
python -m venv nazwa_srodowiska

# Aktywacja środowiska wirtualnego na Windows
nazwa_srodowiska\Scripts\activate.bat

# Aktywacja środowiska wirtualnego na Unix/MacOS
source nazwa_srodowiska/bin/activate

# Instalacja pakietu (przykładowo: requests)
pip install requests

# Tworzenie pliku main.py i uruchomienie pierwszego skryptu
echo 'print("Witaj, świecie!")' > main.py
python main.py
```
Oczekiwane wyjście:
```
Witaj, świecie!
```

## Deep Dive (Dogłębna analiza)
Historia tworzenia nowych projektów w Pythonie ewoluowała — od prostych skryptów do zaawansowanych aplikacji. Używanie środowisk wirtualnych (`venv`) stało się najlepszą praktyką, by uniknąć konfliktów między bibliotekami. Istnieją też narzędzia jak `pipenv` czy `poetry`, które oferują rozszerzone zarządzanie zależnościami. Sam Python ewoluował, dostarczając nowe możliwości z każdą wersją, jak np. deklaratywne adnotacje typów od Python 3.5.

## See Also (Zobacz również)
- [Dokumentacja Pythona](https://docs.python.org/3/)
- [Wprowadzenie do pip](https://pip.pypa.io/en/stable/getting-started/)
- [Poetry - Python dependency management](https://python-poetry.org/docs/)
- [Pipenv: Python Dev Workflow for Humans](https://pipenv.pypa.io/en/latest/)
