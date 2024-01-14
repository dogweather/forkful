---
title:    "C#: Pisanie pliku tekstowego"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest nieodłączną częścią procesu programowania w języku C#. Niezależnie od tego, czy tworzysz prosty program konsolowy czy zaawansowaną aplikację, prawdopodobnie będziesz musiał wczytać lub zapisać dane do pliku tekstowego.

## Jak to zrobić?

Pisanie do pliku tekstowego w języku C# jest bardzo proste i wymaga tylko kilku linijek kodu. W poniższym przykładzie użyjemy klasy `StreamWriter` do utworzenia i zapisania tekstu do pliku:

```C#
using System.IO;

// Tworzymy obiekt StreamWriter i przekazujemy mu nazwę pliku oraz true, aby móc dokonywać zapisu do istniejącego pliku.
StreamWriter sw = new StreamWriter("tekstowy_plik.txt", true);

// Zapisujemy tekst do pliku i zamykamy obiekt StreamWriter.
sw.WriteLine("To jest przykładowy tekst do zapisania do pliku.");
sw.Close();
```

Jeśli chcemy odczytać istniejący plik, możemy użyć klasy `StreamReader`, w następujący sposób:

```C#
using System.IO;

// Tworzymy obiekt StreamReader i przekazujemy mu nazwę pliku.
StreamReader sr = new StreamReader("tekstowy_plik.txt");

// Odczytujemy zawartość pliku linia po linii i wyświetlamy ją w konsoli.
string linia;
while((linia = sr.ReadLine()) != null)
{
    Console.WriteLine(linia);
}
// Zamykamy obiekt StreamReader.
sr.Close();
```

Po uruchomieniu powyższego kodu, w konsoli zobaczymy wyjście zawierające tekst zapisany w pliku.

## Gleboki zanurzenie

W przypadku bardziej zaawansowanych działań związanych z pisanym plików tekstowych, warto zapoznać się z innymi klasami dostępnymi w przestrzeni nazw `System.IO`. Na przykład, aby pracować z plikami z kodowaniem Unicode, należy użyć klasy `StreamWriter` z dodatkowym parametrem, który określa kodowanie. Możemy również wykorzystać klasy `File` lub `FileInfo` do wykonywania różnych operacji, takich jak kopiowanie, przenoszenie czy usuwanie plików.

## Zobacz również

- [Dokumentacja języka C# - Pisanie do pliku tekstowego](https://docs.microsoft.com/pl-pl/dotnet/csharp/programming-guide/file-system/how-to-write-to-a-text-file)
- [Tutorial - Jak pisać do pliku tekstowego w C#](https://www.geeksforgeeks.org/c-sharp-program-write-text-file/)
- [Kurs języka C# - Pisanie do pliku](https://csharp-station.com/Tutorial/CSharp/Lesson23)