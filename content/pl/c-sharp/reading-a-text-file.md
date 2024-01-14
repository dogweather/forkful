---
title:                "C#: Odczytywanie pliku tekstowego"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek chciałeś otworzyć plik tekstowy w swoim programie i wykorzystać jego zawartość? Pisanie kodu, który umożliwi Ci czytanie plików tekstowych może być przydatne w wielu scenariuszach, takich jak przetwarzanie danych lub tworzenie raportów. W tym artykule dowiesz się, jak w łatwy sposób czytać pliki tekstowe w języku C#.

## Jak To Zrobić

W C#, aby czytać pliki tekstowe, musisz najpierw użyć klasy StreamReader, która pozwala na czytanie danych z pliku. Zobacz poniższy kod:

```C#
using System;
using System.IO;

class Program
{
    static void Main(string[] args)
    {
        string filePath = "textFile.txt";
        StreamReader reader = new StreamReader(filePath);

        string fileContents = reader.ReadToEnd();

        Console.WriteLine(fileContents);

        reader.Close();
    }
}
```

Powyższy kod pokazuje, jak możesz użyć klasy StreamReader do czytania pliku tekstowego. Najpierw musisz określić ścieżkę do pliku tekstowego, który chcesz otworzyć, a następnie utworzyć nowy obiekt klasy StreamReader, podając tę ścieżkę jako argument. Następnie użyj metody ReadToEnd(), aby przeczytać całą zawartość pliku i przypisać ją do zmiennej. Wreszcie, możesz wyświetlić zawartość pliku w konsoli i zamknąć obiekt reader.

W powyższym przykładzie użyliśmy metody ReadToEnd(), ale istnieją też inne metody, które możesz wykorzystać do czytania pliku w różnych sposób, takich jak ReadLine(), Read(), czy ReadBlock(). Zapoznaj się z dokumentacją dotyczącą klasy StreamReader, aby poznać pełną listę dostępnych metod.

## Deep Dive

Klasa StreamReader oferuje również wiele opcji konfiguracyjnych, które mogą poprawić wydajność i kontrolę podczas czytania plików tekstowych. Na przykład, możesz określić kodowanie pliku, tryb otwarcia czy obiekt kodujący. Możesz również użyć konstrukcji "using" do automatycznego zamknięcia obiektu klasy StreamReader po użyciu go, co jest częstym i zalecanym sposobem korzystania z tej klasy.

## Zobacz Również

- Dokumentacja dotycząca klasy StreamReader: https://docs.microsoft.com/pl-pl/dotnet/api/system.io.streamreader?view=net-5.0
- Jak pisać do pliku tekstowego w C#: https://kodowanko.pl/jak-pisac-do-pliku-tekstowego-w-c/
- Przetwarzanie plików w języku C#: https://docs.microsoft.com/pl-pl/dotnet/csharp/programming-guide/file-system/how-to-read-from-a-text-file