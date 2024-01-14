---
title:    "C#: Odczytywanie pliku tekstowego"
keywords: ["C#"]
---

{{< edit_this_page >}}

# Dlaczego warto czytać pliki tekstowe w C#

Jeśli jesteś programistą lub uczysz się programowania w C#, być może zastanawiasz się, dlaczego warto poznać technikę czytania plików tekstowych. W tym artykule dowiesz się, jak ważne jest to narzędzie i jak używać go w swoich projektach.

## Jak to zrobić

Aby czytać pliki tekstowe w C#, używamy klasy `StreamReader` z przestrzeni nazw `System.IO`. Najpierw musimy utworzyć nowy obiekt `StreamReader`, podając mu ścieżkę do pliku, który chcemy odczytać. Następnie wywołujemy metodę `ReadLine()`, aby odczytać pojedynczą linię tekstu z pliku. Ten proces powtarzamy dla kolejnych linii, aż do momentu, gdy osiągniemy koniec pliku.

```C#
using System;
using System.IO;

// utworzenie obiektu StreamReader dla pliku "tekstowy.txt"
StreamReader reader = new StreamReader(@"C:\Users\Użytkownik\Desktop\tekstowy.txt");

string line;

// odczytanie i wyświetlenie kolejnej linii tekstu
while ((line = reader.ReadLine()) != null)
{
    Console.WriteLine(line);
}

reader.Close(); // zamknięcie strumienia
```

### Przykładowy plik tekstowy

```
Line 1
Line 2
Line 3
```

### Poniżej znajduje się oczekiwany wynik działania kodu:

```
Line 1
Line 2
Line 3
```

## Deep Dive

Czytanie plików tekstowych jest bardzo przydatne, ponieważ pozwala nam na dostęp do dużej ilości informacji, które mogą być wykorzystane w naszych programach. Ponadto, dzięki tej technice, jesteśmy w stanie przetwarzać i analizować duże zbiory danych.

Ważne jest również, aby zawsze pamiętać o zamknięciu strumienia po zakończeniu operacji na pliku tekstowym. W przeciwnym razie może to spowodować problemy z dostępem do pliku przez inne programy lub nawet utratę danych.

## Zobacz także
- [Dokumentacja klasy StreamReader w C#](https://docs.microsoft.com/pl-pl/dotnet/api/system.io.streamreader)
- [Jakie jeszcze metody możemy wykorzystać do czytania plików tekstowych w C#](https://docs.microsoft.com/pl-pl/dotnet/api/system.io.file)
- [Sposoby na przetwarzanie danych z pliku w C#](https://www.c-sharpcorner.com/article/file-processing-in-c-sharp/)