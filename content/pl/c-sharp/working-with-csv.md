---
title:                "C#: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

Praca z plikami CSV jest częstym zadaniem w programowaniu. Dzięki nim możemy z łatwością przechowywać i przetwarzać dane z różnych źródeł. W tym artykule dowiesz się dlaczego warto uczyć się jak pracować z tym formatem.

## Jak to zrobić

Korzystając z języka C#, możemy łatwo wczytywać, przetwarzać i zapisywać dane w formacie CSV. Przykładowy kod poniżej demonstruje te czynności:

```C#
// Wczytaj dane z pliku CSV do tablicy
string[] lines = File.ReadAllLines("dane.csv"); 

// Przetwórz dane i wyświetl je w konsoli
foreach (string line in lines)
{
    string[] data = line.Split(','); // podziel linię na poszczególne wartości
    Console.WriteLine("Imię: " + data[0] + " | Wiek: " + data[1]); // wyświetl dane w określonym formacie
}

// Zapisz dane w formacie CSV do nowego pliku
List<string> newData = new List<string> { "Anna, 25", "Krzysztof, 32", "Małgorzata, 41" };
File.WriteAllLines("nowe_dane.csv", newData);
```

Powyższy kod wczytuje dane z pliku "dane.csv", przetwarza je i wyświetla w konsoli, a następnie zapisuje zmienione dane do nowego pliku CSV o nazwie "nowe_dane.csv". Bardzo ważne jest także pamiętanie o odpowiednim formatowaniu danych, aby uniknąć błędów przy wczytywaniu lub zapisywaniu pliku.

## Głębsze wchodzenie w temat

Pliki CSV pozwalają nam przechowywać dane w formie tabelarycznej, co jest bardzo przydatne w wielu przypadkach. W języku C# możemy skorzystać z różnych metod, takich jak odczytywanie tylko wybranych kolumn, filtrowanie danych, czy też sortowanie. Dzięki temu mamy pełną kontrolę nad przetwarzanymi danymi.

Ponadto, ważne jest także przestrzeganie zasad konwencji dla plików CSV. Na przykład, warto pamiętać o kodowaniu znaków, w przypadku gdy w pliku występują znaki specjalne, oraz o formatowaniu liczb i dat, aby uniknąć nieporozumień przy przetwarzaniu danych.

## Zobacz także

- Dokumentacja języka C# na temat CSV: [https://docs.microsoft.com/pl-pl/dotnet/csharp/csv/](https://docs.microsoft.com/pl-pl/dotnet/csharp/csv/)
- Przykładowa biblioteka do obsługi plików CSV w C#: [https://www.codeproject.com/Articles/415732/C-CSV-Reader](https://www.codeproject.com/Articles/415732/C-CSV-Reader)
- Wideo-tutorial na temat pracy z plikami CSV w C#: [https://www.youtube.com/watch?v=NX-euMnmnfE](https://www.youtube.com/watch?v=NX-euMnmnfE)