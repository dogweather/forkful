---
title:    "C#: Odczytywanie pliku tekstowego"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą lub chcesz zostać programistą, przeczytanie pliku tekstowego jest ważnym umiejętnością, która może być wykorzystana w wielu projektach. Niezależnie od tego, czy chcesz wczytać dane do swojej aplikacji, czy potrzebujesz odczytać dane z pliku konfiguracyjnego, znajomość operacji na plikach jest niezbędna w świecie programowania.

## Jak to zrobić

Aby wczytać plik tekstowy za pomocą C#, musisz najpierw otworzyć strumień danych do odczytu pliku. Możesz to zrobić za pomocą klasy File i metody OpenText, która zwraca obiekt StreamReader.

```C#
string filePath = "sciezka/do/pliku.txt";
var fileStream = File.OpenText(filePath);
```

Możesz także skorzystać z konstrukcji using, która automatycznie zamknie strumień po zakończeniu operacji.

```C#
using (var fileStream = File.OpenText(filePath))
{
    // kod odczytujący plik
}
```

Następnie możesz użyć metody ReadLine, aby odczytać kolejne linie z pliku. Poniżej znajduje się przykładowy kod, który odczytuje linie z pliku i wyświetla je na ekranie.

```C#
using (var fileStream = File.OpenText(filePath))
{
    string line;
    while((line = fileStream.ReadLine()) != null)
    {
        Console.WriteLine(line);
    }
}
```

Możesz także użyć metody ReadAllText, jeśli chcesz odczytać cały plik na raz i zapisać go do zmiennej typu string.

```C#
string fileContents = File.ReadAllText(filePath);
```

## Głębszy wgląd

Aby lepiej zrozumieć jak działa odczytywanie plików tekstowych w C#, warto zapoznać się z klasy StreamReader, która odpowiada za odczyt danych ze strumienia. Możesz także wykorzystać metody takie jak Read, ReadBlock czy ReadToEnd, aby dokładniej kontrolować odczytywane dane.

Pamiętaj także o odpowiednim obsłużeniu wyjątków, które mogą wystąpić podczas operacji na plikach. Najczęściej spotykane błędy to brak dostępu do pliku lub próba odczytania pliku, który nie istnieje.

## Zobacz także

- [Dokumentacja klasy File w C# (Microsoft Docs)](https://docs.microsoft.com/pl-pl/dotnet/api/system.io.file?view=netcore-3.1)
- [Kurs C# (W3Schools)](https://www.w3schools.com/cs/default.asp)
- [Wprowadzenie do programowania w języku C# (YouTube)](https://www.youtube.com/watch?v=zW6Ab_XpqCc)