---
title:    "C#: Sprawdzanie istnienia katalogu"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Dlaczego

Sprawdzenie, czy katalog istnieje, jest ważnym krokiem w wielu projektach programistycznych. Pozwala to upewnić się, że ścieżki i pliki są poprawnie udostępnione i mogą być używane przez aplikację. Jest to podstawowa czynność, która jest niezbędna w wielu sytuacjach.

## Jak to zrobić?

Sprawdzenie, czy katalog istnieje, jest bardzo proste w języku C#. Wystarczy użyć metody statycznej "Directory.Exists" i przekazać jej ścieżkę do sprawdzenia. Poniżej znajduje się przykładowy kod C#:

```C#
// Sprawdzenie, czy katalog "Documents" istnieje w folderze użytkownika
string path = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments);
if (Directory.Exists(path))
{
    Console.WriteLine("Katalog 'Documents' istnieje.");
}
else
{
    Console.WriteLine("Katalog 'Documents' nie istnieje.");
}
```

Powyższy kod jest bardzo prosty i może zostać dostosowany do różnych celów. W przypadku gdy katalog nie istnieje, można wykonać kod, który go utworzy, lub wyświetlić odpowiednie komunikaty dla użytkownika.

## Głębsze zagadnienia

Sprawdzenie istnienia katalogu może być użyteczne także w innych sytuacjach. Na przykład, można użyć tej metody do sprawdzenia, czy dany katalog nie jest już używany przez inny proces. Można także użyć tej metody do odnalezienia konkretnych katalogów w strukturze plików lub do zabezpieczenia programu przed próbą dostępu do nieistniejących katalogów.

Jednak warto pamiętać, że ta metoda tylko sprawdza, czy katalog istnieje, a nie czy jest dostępny lub czy użytkownik ma odpowiednie uprawnienia do jego używania. W przypadku bardziej zaawansowanej walidacji, należy użyć innych metod lub kombinacji metod, aby dokładniej sprawdzić stan katalogu.

## Zobacz także

- [Metoda Directory.Exists w dokumentacji Microsoft](https://docs.microsoft.com/pl-pl/dotnet/api/system.io.directory.exists?view=net-5.0)
- [Przykłady użycia metody Directory.Exists w języku C#](https://www.c-sharpcorner.com/blogs/check-if-folder-exists-in-c-sharp1)