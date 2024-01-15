---
title:                "Tworzenie pliku tekstowego"
html_title:           "Python: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie pliku tekstowego jest niezwykle przydatne w programowaniu w języku Python. Pozwala ono na zapisywanie danych w sposób trwały i łatwy do odczytania przez komputer.

## Jak to zrobić

Pisanie plików tekstowych w języku Python jest bardzo proste. Wystarczy użyć wbudowanych funkcji do obsługi plików.

Najpierw musimy otworzyć plik w trybie "write" (zapis). Aby to zrobić, użyjmy funkcji `open()` z dwoma argumentami: nazwą pliku oraz trybem "w". Następnie możemy użyć metody `write()` aby napisać zawartość do pliku. Pamiętajmy również o zamknięciu pliku za pomocą metody `close()`.

Przykład:

```Python
# otwieramy plik
plik = open("moj_plik.txt", "w")

# zapisujemy zawartość do pliku
plik.write("Witaj wśród Pythonowiczów!")

# zamykamy plik
plik.close()
```

## Zagłębienie

Możemy również użyć instrukcji `with` do otwarcia i zamknięcia pliku w trochę inny sposób.

Przykład:

```Python
# otwieramy plik i nadajemy mu nazwę "plik"
with open("moj_plik.txt", "w") as plik:
    # możemy wykonać dowolne operacje na pliku pomiędzy tymi instrukcjami
    plik.write("Witaj wśród Pythonowiczów!")

# plik został automatycznie zamknięty po wyjściu z instrukcji "with"
```

Jeśli chcemy odczytać zawartość pliku, możemy również użyć funkcji `open()` z argumentem "r" (czytanie) oraz metody `read()` aby odczytać tekst ze wskazanego pliku.

Ostatnią ważną rzeczą o której należy pamiętać, jest ścieżka pliku. Gdy nie podamy pełnej ścieżki, plik zostanie utworzony w bieżącym folderze, gdzie znajduje się nasz skrypt.

## Zobacz również

- Dokumentacja języka Python - https://docs.python.org/pl/3/tutorial/inputoutput.html
- Szczegółowy poradnik o plikach w Pythonie - https://www.python.pl/poradniki/podcast-files/
- Przykładowe projekty na GitHubie wykorzystujące pisanie plików w Pythonie - https://github.com/topics/python-file-processing