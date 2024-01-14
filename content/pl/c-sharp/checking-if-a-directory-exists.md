---
title:    "C#: Sprawdzanie istnienia katalogu"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu, niezbędne jest sprawdzanie czy określony katalog istnieje, zanim spróbujemy wykonać na nim jakieś operacje. Jest to ważny krok, ponieważ w przypadku braku katalogu, nasz program może rzucić błąd i przestać działać. W dzisiejszym artykule przeanalizujemy, jak w prosty sposób możemy sprawdzić istnienie katalogu w języku C#.

## Jak to zrobić

W języku C# możemy skorzystać z metody `Directory.Exists()` aby sprawdzić czy dany katalog istnieje. Przykładowy kod wykorzystujący tę metodę wygląda następująco:

```C#
string path = @"C:\Users\Username\Documents\ExampleFolder";

if(Directory.Exists(path))
{
    Console.WriteLine("Katalog istnieje.");
}
else
{
    Console.WriteLine("Katalog nie istnieje.");
}

```

W powyższym przykładzie ustalamy zmienną `path` na ścieżkę do katalogu, który chcemy sprawdzić. Następnie, w warunku `if` korzystamy z metody `Directory.Exists()` i podajemy jej naszą zmienną `path`. Jeżeli metoda zwróci wartość `true`, wypisujemy na ekranie informację o istnieniu katalogu, w przeciwnym razie, wypisujemy informację o jego braku.

Jeśli chcielibyśmy wyświetlić przykładowe wyjście, gdy katalog istnieje i gdy nie istnieje, możemy skorzystać z poniższego kodu:

```C#
string path = @"C:\Users\Username\Documents\ExampleFolder";

if(Directory.Exists(path))
{
    Console.WriteLine("Katalog istnieje.");
    string[] files = Directory.GetFiles(path);
    Console.WriteLine("Pliki w katalogu:");
    foreach(string file in files)
    {
        Console.WriteLine(file);
    }
}
else
{
    Console.WriteLine("Katalog nie istnieje.");
}
```

W powyższym kodzie, po informacji o istnieniu katalogu, wyświetlamy listę plików znajdujących się wewnątrz naszego katalogu. Możemy oczywiście dowolnie zmieniać i modyfikować kod w zależności od swoich potrzeb.

## Deep Dive

Metoda `Directory.Exists()` ma zaimplementowany mechanizm pozwalający wewnętrznie wywołać funkcję systemową do sprawdzania istnienia katalogu. Dzięki temu nie musimy samodzielnie implementować tej funkcji i możemy skupić się na pozostałej części naszego kodu.

Warto również zauważyć, że metoda ta ma wiele praktycznych zastosowań. Możemy jej użyć do sprawdzania istnienia katalogu na dysku twardym, ale również na serwerze lub w chmurze, jeśli jesteśmy w trakcie tworzenia aplikacji internetowej.

## Zobacz także

- [Dokumentacja metody Directory.Exists() w języku C#](https://docs.microsoft.com/pl-pl/dotnet/api/system.io.directory.exists?view=netframework-4.8)
- [Poradnik: Tworzenie i usuwanie katalogów w języku C#](https://docs.microsoft.com/pl-pl/dotnet/standard/io/how-to-create-and-delete-a-directory-folder)
- [Artykuł: Sprawdzanie dostępności plików i katalogów w języku C#](https://www.c-sharpcorner.com/UploadFile/puranindia/checking-files-and-folders-by-using-C-Sharp-code/)