---
title:                "Korzystanie z interaktywnego shella (REPL)"
aliases:
- /pl/python/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:17:14.218478-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z interaktywnego shella (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
REPL, czyli Pętla Czytaj-Wykonaj-Drukuj, to środowisko programistyczne, które przyjmuje pojedyncze wejścia od użytkownika, wykonuje je i zwraca wynik użytkownikowi. Programiści używają go do szybkich testów, nauki, debugowania, czy wykonania obliczeń "na bieżąco".

## Jak to zrobić:
Wejdź od razu do REPL Pythona, wpisując `python` w wierszu poleceń. Gdy tam będziesz, przetestuj proste operacje lub kod wieloliniowy:

```Python
>>> 1 + 1
2
>>> for i in range(3):
...     print(i)
... 
0
1
2
```

Eksperymentuj z funkcjami i natychmiastową odpowiedzią:

```Python
>>> def greet(name):
...     return "Cześć, " + name + "!"
... 
>>> greet("Alice")
'Cześć, Alice!'
```

Zabawiaj się bibliotekami i eksploruj ich funkcje w czasie rzeczywistym:

```Python
>>> import math
>>> math.sqrt(16)
4.0
```

Wyjdź szybko za pomocą `exit()` lub `Ctrl+D` (czasami `Ctrl+Z` na Windowsie).

## Pogłębiona analiza
Koncepcja REPL nie jest unikalna dla Pythona; jest tak stara jak Lisp. Wiele języków oferuje to natychmiastowe, interaktywne środowisko dla praktycznego podejścia do kodowania. Alternatywy dla natywnej powłoki Pythona obejmują IPython i Jupyter Notebook, które zapewniają lepszą interaktywność, więcej funkcji i lepszą integrację z innymi narzędziami. Standardowy REPL Pythona jest prosty, ale zawiera pełną moc Pythona, obsługując skomplikowane obiekty i programy wielowątkowe, choć brakuje mu funkcji takich jak auto-uzupełnianie i podświetlanie składni, które są obecne w bardziej zaawansowanych narzędziach.

## Zobacz również
- [Oficjalna dokumentacja Pythona dotycząca interpretera](https://docs.python.org/3/tutorial/interpreter.html)
- [IPython: Zaawansowana powłoka Pythona](https://ipython.org/)
- [Projekt Jupyter](https://jupyter.org/)
