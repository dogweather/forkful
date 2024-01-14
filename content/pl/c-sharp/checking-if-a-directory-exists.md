---
title:                "C#: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

W tym wpisie omówimy sprawdzanie istnienia katalogów w języku C#. Jest to ważna umiejętność, która może pomóc programistom w stworzeniu bardziej niezawodnych i bezpiecznych aplikacji. Dodatkowo, umiejętność sprawdzania istnienia katalogów jest również niezbędna w celu uniknięcia błędów w trakcie uruchamiania programów.

## Jak to zrobić

Sprawdzanie istnienia katalogów w C# jest bardzo proste i można to zrobić za pomocą metody statycznej "Directory.Exists". Poniżej znajduje się przykładowy kod, który sprawdza czy katalog "C:\Users" istnieje:

```C#
if (Directory.Exists(@"C:\Users"))
{
    Console.WriteLine("Katalog istnieje.");
}
else
{
    Console.WriteLine("Katalog nie istnieje.");
}
```

Przykładowy output w przypadku istnienia katalogu:

```
Katalog istnieje.
```

Przykładowy output w przypadku braku katalogu:

```
Katalog nie istnieje.
```

## Głębsza analiza

Metoda "Directory.Exists" zwraca wartość logiczną "true" lub "false" w zależności od istnienia lub braku katalogu. Może być również używana do sprawdzania istnienia plików, wystarczy podać ścieżkę do pliku zamiast ścieżki do katalogu.

Warto również pamiętać, że metoda ta może być również używana do sprawdzania istnienia katalogów na innych dyskach, należy tylko podać odpowiednią ścieżkę. Można również użyć metody "File.Exists" w przypadku sprawdzania istnienia plików.

## Zobacz również

- [Praca z plikami i katalogami w C#](https://docs.microsoft.com/pl-pl/dotnet/api/system.io.directory.exists?view=netcore-3.1)
- [10 przykładów sprawdzania istnienia plików i katalogów w C#](https://www.c-sharpcorner.com/UploadFile/9a7742/checking-directory-and-file-exists-in-C-Sharp/)