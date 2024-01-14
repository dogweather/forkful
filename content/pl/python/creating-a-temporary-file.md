---
title:    "Python: Tworzenie pliku tymczasowego"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego?

Stworzenie tymczasowego pliku jest bardzo przydatne w przypadku, gdy chcemy tymczasowo przechować lub przetworzyć dane w naszym kodzie. Jest to szczególnie przydatne, gdy pracujemy z dużymi ilościami danych lub musimy wykonać operacje na plikach, które mogą zawierać wrażliwe informacje.

## Jak to zrobić?

Aby stworzyć tymczasowy plik w Pythonie, możemy skorzystać z wbudowanej biblioteki "tempfile". W poniższym przykładzie pokazane jest jak utworzyć tymczasowy plik, zapisać w nim dane i odczytać je ponownie:

```Python
import tempfile

# Tworzymy tymczasowy plik
temp_file = tempfile.NamedTemporaryFile()

# Zapisujemy dane do pliku
temp_file.write(b"Hello World!")

# Odczytujemy zawartość pliku
temp_file.seek(0) # Wraca na początek pliku
contents = temp_file.read()

# Wypisujemy wynik
print(contents)

# Pamiętaj o zamknięciu pliku po ukończonym działaniu
temp_file.close()
```

Przykładowy output:
```
b'Hello World!'
```

Możemy również wskazać prefiks i sufiks nazwy tymczasowego pliku oraz określić w jakim trybie chcemy otworzyć plik. Więcej informacji na ten temat znajduje się w dokumentacji biblioteki "tempfile".

## Głębszy zanurzenie

Tworzenie tymczasowego pliku jest bardzo ważne ze względu na bezpieczeństwo i efektywność naszego kodu. W przypadku gdy pracujemy z wrażliwymi danymi, nie chcemy, aby pozostały one na stałe na naszym dysku. Dzięki użyciu tymczasowych plików, mamy pewność, że po zakończeniu działania naszego programu, wszystkie dane zostaną usunięte.

Dodatkowo, tworzenie tymczasowych plików może również przyspieszyć nasz kod. W przypadku dużych plików, operacje na nich mogą zajmować dużo czasu. Korzystając z tymczasowych plików, wykorzystujemy pamięć podręczną systemu operacyjnego, co pozwala na szybsze odczyty i zapisy danych.

## Zobacz też

- Dokumentacja biblioteki "tempfile": https://docs.python.org/3/library/tempfile.html
- Wprowadzenie do Pythona: https://www.python.org/about/gettingstarted/
- Tworzenie tymczasowych plików w różnych językach programowania: https://stackabuse.com/creating-temporary-files-in-python/