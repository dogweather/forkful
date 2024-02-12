---
title:                "Pisanie pliku tekstowego"
aliases:
- /pl/c-sharp/writing-a-text-file/
date:                  2024-02-03T19:27:39.528908-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pisanie pliku tekstowego"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie pliku tekstowego w C# wiąże się z programowym tworzeniem lub modyfikowaniem plików tekstowych w systemie plików - podstawowym zadaniem dla wielu aplikacji, takich jak logowanie, eksport danych czy zarządzanie konfiguracją. Programiści wykonują tę operację w celu zachowania danych między sesjami, udostępnienia informacji między systemami lub zapisania wyników w formie czytelnej dla człowieka.

## Jak to zrobić:
C# ułatwia operacje na plikach dzięki przestrzeni nazw `System.IO`, oferującej proste metody do pisania plików tekstowych. Oto jak napisać podstawowy plik tekstowy i dodać tekst do istniejącego pliku.

### Pisanie do pliku tekstowego od zera
```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string content = "Hello, world!";

        // Zapisanie zawartości do nowego pliku
        File.WriteAllText(filePath, content);
        
        Console.WriteLine("Plik zapisany pomyślnie.");
    }
}
```
**Przykładowy wynik:**
```
Plik zapisany pomyślnie.
```

### Dodawanie tekstu do istniejącego pliku
Jeśli chcesz dodać tekst na końcu istniejącego pliku, możesz użyć metody `File.AppendAllText`.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string additionalContent = "\nDodaję więcej treści.";

        // Dodanie zawartości do pliku
        File.AppendAllText(filePath, additionalContent);
        
        Console.WriteLine("Treść dodana pomyślnie.");
    }
}
```
**Przykładowy wynik:**
```
Treść dodana pomyślnie.
```

### Używanie bibliotek firm trzecich: `StreamWriter`
Aby uzyskać większą kontrolę nad pisaniem, w tym automatyczne opróżnianie bufora i wybór kodowania, użyj `StreamWriter`.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string content = "To jest przykład z użyciem StreamWriter.";

        // Używanie StreamWriter do zapisywania do pliku
        using (StreamWriter writer = new StreamWriter(filePath, append: true))
        {
            writer.WriteLine(content);
        }
        
        Console.WriteLine("Plik zapisany za pomocą StreamWriter pomyślnie.");
    }
}
```
**Przykładowy wynik:**
```
Plik zapisany za pomocą StreamWriter pomyślnie.
```

Każde z tych podejść służy innym potrzebom: bezpośrednie metody `File` dla szybkich operacji i `StreamWriter` dla bardziej złożonych scenariuszy pisania. Wybierz opcję bazując na swoich konkretnych wymaganiach, biorąc pod uwagę czynniki takie jak wydajność i rozmiar pliku.
