---
title:                "Rozpoczynanie nowego projektu"
html_title:           "Python: Rozpoczynanie nowego projektu"
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "Python"
category:             "Python"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Zaczęcie nowego projektu to proces, w którym programiści tworzą nowe programy lub rozszerzają istniejące o nowe funkcje. Przy okazji tworzą kod, rozwiązują problemy i uczą się nowych rzeczy. Robią to, aby rozwijać swoje umiejętności i przyczynić się do rozwoju technologicznego.

## Jak to zrobić:
```Python
# Przykładowy kod do rozpoczęcia nowego projektu
import os

# Utworzenie nowego folderu dla projektu
os.mkdir("Nowy_projekt")

# Utworzenie nowego pliku z kodem
with open("nowy_projekt.py", "w") as file:
    file.write("print('Witaj w moim nowym projekcie!')")
    
# Uruchomienie nowego pliku z kodem
os.system("python nowy_projekt.py")
```

Przykładowy output:
```
Witaj w moim nowym projekcie!
```

## Deep Dive:
W przeszłości, tworzenie nowych projektów było bardziej czasochłonne i wymagało wielu działań, takich jak ręczna konfiguracja środowiska i tworzenia wszystkich plików od zera. Dzięki narzędziom jak Django czy Flask, można teraz szybko rozpocząć nowe projekty i skupić się na ich rozwoju. Alternatywą dla języka Python mogą być na przykład C++ lub Java. W celu rozpoczęcia nowego projektu należy mieć podstawową wiedzę na temat programowania w języku Python oraz umieć korzystać z narzędzi takich jak Git czy Virtualenv.

## Zobacz też:
- [Django](https://www.djangoproject.com/)
- [Flask](https://flask.palletsprojects.com/)
- [Virtualenv](https://virtualenv.pypa.io/en/latest/)
- [Git](https://git-scm.com/)