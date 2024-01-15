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

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak programy czytają pliki tekstowe? Czy chciałbyś nauczyć się, jak łatwo i skutecznie odczytać plik tekstowy w Pythonie? Jeśli tak, to ten artykuł jest dla Ciebie!

## Jak to zrobić

```Python
# Otwieramy plik tekstowy w trybie odczytu
file = open("tekstowy_plik.txt", "r")

# Czytamy całą zawartość pliku
data = file.read()

# Wyświetlamy zawartość pliku
print(data)

# Zamykamy plik
file.close()
```
Przykładowy plik tekstowy może wyglądać tak:
```
To jest przykładowy plik tekstowy, który będzie czytany przez nasz program. 
Może zawierać różne linijki tekstu, symbole, liczby czy też puste linie. 
To nie ma znaczenia, ponieważ nasz program jest w stanie odczytać go bez problemu. 
```

Po uruchomieniu powyższego skryptu, otrzymamy na konsoli następujące wyjście:
```
To jest przykładowy plik tekstowy, który będzie czytany przez nasz program. 
Może zawierać różne linijki tekstu, symbole, liczby czy też puste linie. 
To nie ma znaczenia, ponieważ nasz program jest w stanie odczytać go bez problemu. 
```

W ten sposób w prosty sposób możemy odczytać całą zawartość pliku tekstowego przy pomocy Pythona.

## Deep Dive

Podczas czytania pliku tekstowego przy pomocy Pythona, istnieją różne tryby dostępu, w których może on zostać otwarty. W przykładzie powyżej użyliśmy trybu "r", który oznacza "odczyt". Istnieją również inne tryby, takie jak "w" (zapis), "a" (dopisywanie), "x" (tworzenie), czy "rb" (odczyt w trybie binarnym). Więcej informacji o tych trybach można znaleźć w dokumentacji Pythona.

Podczas czytania pliku tekstowego, możemy również ustalić punkt, w którym chcemy rozpocząć lub zakończyć czytanie. Dzieje się to za pomocą metody `read()`, w której jest możliwe podanie liczby bajtów, które chcemy odczytać. Na przykład, jeśli podamy `file.read(10)`, otrzymamy tylko pierwsze 10 bajtów pliku tekstowego.

See Also
- [Dokumentacja Pythona](https://docs.python.org/pl/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Wprowadzenie do czytania plików w Pythonie](https://realpython.com/read-write-files-python/#reading-files-in-python)
- [Czytanie i zapisywanie plików tekstowych w Pythonie](https://www.w3schools.com/python/python_file_handling.asp)