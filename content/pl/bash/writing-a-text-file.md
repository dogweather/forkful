---
title:                "Bash: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego piszemy pliki tekstowe?

Pisanie plików tekstowych jest nieodłączną częścią programowania w Bash. Pozwala ono na tworzenie skryptów, a także na zapisywanie wszelkich komunikatów czy danych wyjściowych działającego kodu. Wiele zadań, które wykonujemy w systemie operacyjnym, wymaga również korzystania z plików tekstowych.

## Jak to zrobić?

Pierwszym krokiem w tworzeniu plików tekstowych jest uruchomienie edytora tekstu, na przykład Vi. Następnie należy stworzyć nowy plik z rozszerzeniem .txt, a po zapisaniu wprowadzonych zmian, plik będzie gotowy do użycia. 

Przykładowy kod, który umożliwia stworzenie i zapisanie pliku tekstowego, wygląda następująco:


```Bash
# Tworzenie i otwieranie pliku tekstowego
touch nowy_plik.txt
vi nowy_plik.txt

# Przykładowa treść do zapisania
"Mój pierwszy plik tekstowy!"

# Zapisywanie pliku
:wq
```

Po uruchomieniu tego kodu, w bieżącym folderze zostanie stworzony plik o nazwie "nowy_plik.txt", w którym znajduje się zadany tekst. 

## Dogłębna analiza

Pliki tekstowe są jednym z podstawowych sposobów reprezentacji informacji w systemie pracy Bash. Pozwalają one na przechowywanie i przetwarzanie danych, dzięki czemu są niezbędne w wielu aspektach programowania.

W pliku tekstowym można zapisywać różnego rodzaju dane, począwszy od prostych komunikatów, a skończywszy na złożonych dokumentach czy raportach. Jest to uniwersalny format, który może być czytany przez różne programy, a jego treść może być łatwo edytowana przez użytkownika.

Dodatkową zaletą plików tekstowych jest możliwość przekazywania danych między różnymi programami, co jest niezwykle przydatne w przypadku skryptów Bash. Dzięki temu, możliwe jest w prosty sposób przetworzenie danych wyjściowych jednego skryptu i wykorzystanie ich w kolejnym.

## Zobacz także

- [Pisanie skryptów w Bash](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Podstawy pracy z plikami tekstowymi w Ubuntu](https://www.ubuntu.com/tutorials/command-line-for-beginners#1-overview)