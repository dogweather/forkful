---
title:                "Odczytywanie pliku tekstowego"
html_title:           "Python: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Czytanie pliku tekstowego jest jednym ze sposobów na odczytanie zawartości tekstu z systemu plików. Programiści często używają tej metody, aby przetwarzać duże ilości tekstu lub pliki z danymi, które muszą zostać wczytane do programu.

## Jak to zrobić:
Przykłady kodu i wyników znajdziesz poniżej. Pamiętaj jednak, że może to się różnić w zależności od systemu operacyjnego i konfiguracji środowiska.

```Python
# Otwarcie pliku i odczytanie zawartości
file = open("przykladowy_plik.txt", "r") # funkcja open() otwiera plik w trybie tylko do odczytu (read-only)
print(file.read()) # funkcja read() wczytuje zawartość pliku i zwraca ją jako łańcuch znaków
file.close() # pamiętaj o zamknięciu pliku po wykonaniu operacji

# Można także odczytywać plik linia po linii
file = open("przykladowy_plik.txt", "r")
for line in file:
    print(line)
file.close()

# Aby odczytać plik ze specjalnymi znakami, trzeba użyć modułu codecs
import codecs
file = codecs.open("przykladowy_plik.txt", "r", encoding="utf-8")
print(file.read())
file.close()
```

## Głębsze zanurzenie:
Czytanie plików tekstowych jest powszechną czynnością w programowaniu już od początków. Wcześniej użycie funkcji open() wymagało określenia trybu dostępu do pliku, jednak w nowszych wersjach języka Python jest to opcjonalne. Alternatywnym sposobem na odczytanie pliku jest wykorzystanie metody readlines(), która zwraca listę zawierającą kolejne linie tekstu. Aby uzyskać dostęp do zawartości pliku, najlepiej używać kontekstowych menedżerów (ang. context managers), które automatycznie zamykają plik po wyjściu z bloku kodu.

## Zobacz też:
- Oficjalna dokumentacja Pythona: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- Przykładowe zadania związane z czytaniem plików: https://www.practicepython.org/exercise/2014/12/06/22-read-from-file.html