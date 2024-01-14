---
title:                "Python: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego tworzyć tymczasowy plik?

Tworzenie tymczasowych plików jest częstym zadaniem w programowaniu, szczególnie w języku Python. Służą one do przechowywania danych lub wykonywania operacji na nich w sposób tymczasowy, bez konieczności trwałego zapisywania ich na dysku. Jest to przydatne, gdy potrzebujemy szybkiego i efektywnego sposobu przechowania danych tylko na czas wykonywania programu.

## Jak to zrobić?

W Pythonie istnieje kilka sposobów na utworzenie tymczasowego pliku. Jednym z nich jest użycie modułu "tempfile", który zapewnia funkcje do tworzenia i obsługi plików tymczasowych. Poniżej przedstawiono przykładowy kod wykorzystujący ten moduł:

```Python
import tempfile

# Tworzenie tymczasowego pliku
with tempfile.NamedTemporaryFile() as temp:
  # Wyświetlenie nazwy tymczasowego pliku
  print("Nazwa tymczasowego pliku:", temp.name)

  # Zapisanie danych do tymczasowego pliku
  temp.write(b"Przykładowe dane")

  # Odczytanie danych z tymczasowego pliku
  temp.seek(0)
  data = temp.read()

# Wyświetlenie odczytanych danych
print("Dane z tymczasowego pliku:", data)
```

W powyższym kodzie najpierw importujemy moduł "tempfile". Następnie wewnątrz bloku "with" tworzymy tymczasowy plik za pomocą funkcji "NamedTemporaryFile()". Po utworzeniu pliku, wyświetlamy jego nazwę i zapisujemy do niego przykładowe dane. Następnie odczytujemy dane z pliku oraz zamykamy go. Na koniec wyświetlamy odczytane dane.

Po wykonaniu powyższego kodu, w konsoli powinny pojawić się następujące wyniki:

```
Nazwa tymczasowego pliku: /tmp/tmp38cph403
Dane z tymczasowego pliku: b'Przykładowe dane'
```

## Głębsze spojrzenie

Podczas tworzenia tymczasowego pliku za pomocą modułu "tempfile", możemy wykorzystać różne opcje, aby dostosować jego działanie. Przykładowe opcje to m.in. nazwa pliku, tryb dostępu, czy też katalog, w którym zostanie utworzony plik. W przypadku, gdy chcemy mieć większą kontrolę nad tymczasowym plikiem, możemy użyć funkcji "mkstemp", która zwróci wskaźnik do pliku oraz jego nazwę w formie łańcucha znaków.

Poniżej przedstawiono przykładowy kod wykorzystujący funkcję "mkstemp":

```Python
import tempfile

# Tworzenie tymczasowego pliku z własną nazwą
temp_name = "moj_tymczasowy_plik.txt"
fd, name = tempfile.mkstemp(suffix=".txt", prefix=temp_name)

# Wyświetlenie wskaźnika oraz nazwy pliku
print("Wskaźnik do pliku:", fd)
print("Nazwa tymczasowego pliku:", name)

# Zapisanie i odczytanie danych z pliku
with open(fd, "w") as temp:
  temp.write("Przykładowe dane")

with open(name, "r") as temp:
  data = temp.read()

# Wyświetlenie odczytanych danych
print("Dane z tymczasowego pliku:", data)

# Usunięcie tymczasowego pliku
temp.close()
```

Podobnie jak w poprzednim przykładzie, najpierw importujemy moduł "tempfile". Następnie wywołujemy funkcję "mkstemp", podając prefiks oraz sufiks nazwy tymczasowego pliku. Funkcja ta zwraca nam wskaźnik oraz nazwę pliku, które następnie możemy wy