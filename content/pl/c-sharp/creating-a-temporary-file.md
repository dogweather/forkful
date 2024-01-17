---
title:                "Tworzenie pliku tymczasowego"
html_title:           "C#: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Czym jest tworzenie tymczasowych plików i dlaczego programiści to robią?

Tworzenie tymczasowych plików to proces tworzenia pliku, który jest używany tylko tymczasowo i jest usuwany, gdy nie jest już potrzebny. Programiści często tworzą tymczasowe pliki do tymczasowego przechowywania danych lub do wykonywania operacji, które wymagają fizycznego pliku na dysku.

Jak to zrobić:

```C#
// Tworzenie tymczasowego pliku za pomocą klasy Path
string tempFile = Path.GetTempFileName();

// Utworzenie nowego tamczasowego pliku o określonej nazwie
string tempFile = Path.GetTempPath() + "moj_tymczasowy_plik.txt";

// Utworzenie tymczasowego pliku przez utworzenie instancji klasy FileStream
FileStream tempFile = new FileStream(Path.GetTempFileName(), FileMode.Create);
```

Podgląd tworzenia i usuwania tymczasowego pliku:

```C#
// Tworzenie tymczasowego pliku
string tempFile = Path.GetTempFileName();

// Wyswietlenie nazwy utworzonego pliku
Console.WriteLine(tempFile);

// Usunięcie tymczasowego pliku
File.Delete(tempFile);

// Wyswietlenie komunikatu o braku pliku
Console.WriteLine(File.Exists(tempFile)); // Output: False 
```

Głębsza analiza:

1. Kontekst historyczny: Tworzenie tymczasowych plików jest powszechnie używane w programowaniu od dawna, a jeszcze przed pojawieniem się języka C#.
2. Alternatywy: Istnieją różne sposoby tworzenia tymczasowych plików w języku C#, jednak wykorzystanie klasy Path jest najpopularniejsze i polecane przez ekspertów.
3. Szczegóły implementacji: Klasa Path zawiera wiele przydatnych metod do zarządzania plikami i ścieżkami, w tym również tworzenie tymczasowych plików.

Zobacz również:

- Dokumentacja klasy Path w języku C#: https://docs.microsoft.com/pl-pl/dotnet/api/system.io.path?view=netcore-3.1
- Porównanie różnych sposobów tworzenia tymczasowych plików w C#: https://stackoverflow.com/questions/1181421/generating-a-unique-temporary-file-name-in-c-sharp