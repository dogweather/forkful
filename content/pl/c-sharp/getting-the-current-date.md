---
title:                "C#: Uzyskiwanie bieżącej daty"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli programujesz w języku C#, prawdopodobnie często potrzebujesz aktualnej daty w swoim kodzie. Mogą być różne powody, na przykład potrzeba wyświetlenia aktualnego czasu w aplikacji lub sprawdzenia czy dany plik został ostatnio zmodyfikowany. Dlatego dzisiaj przyjrzymy się temu, jak pobrać aktualną datę w języku C#.

## Jak To Zrobić

```C#
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate);
```

Kod powyżej pokazuje najprostszy sposób na uzyskanie aktualnej daty w języku C#. Najpierw tworzymy obiekt typu `DateTime` o nazwie `currentDate` używając metody `DateTime.Now`, która zwraca bieżącą datę i czas. Następnie wyświetlamy tę datę na ekranie za pomocą `Console.WriteLine`.

Jeśli chcemy wyświetlić jedynie datę bez czasu, możemy użyć metody `DateTime.Today`. Kod będzie wyglądał tak:

```C#
DateTime currentDate = DateTime.Today;
Console.WriteLine(currentDate);
```

W tym przypadku metoda `DateTime.Today` również zwraca obiekt typu `DateTime`, ale z ustawionym czasem na północ (00:00:00). Jeśli chcesz wyświetlić datę w innym formacie, możesz skorzystać z metody `ToString()` i podać odpowiedni format jako parametr. Na przykład:

```C#
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate.ToString("dd/MM/yyyy"));
```

Wynik powyższego kodu będzie wyświetlał datę w postaci "dd/MM/yyyy" (np. 26/08/2021).

## Deep Dive

Pobieranie aktualnej daty w języku C# może być nieco bardziej skomplikowane, jeśli potrzebujesz wykonać jakieś operacje związane z czasem, na przykład sprawdzenia różnicy między dwoma datami. W takich przypadkach warto poznać pewne przydatne metody z klasy `DateTime`, takie jak `Add()`, `Subtract()` czy `Compare()`.

Możesz też poszerzyć swoją wiedzę o datach i czasie w C#, korzystając z klasy `TimeSpan`, która reprezentuje określony interwał czasowy. W połączeniu z klasą `DateTime`, możesz wykonywać różnego rodzaju operacje na datach i czasach, na przykład obliczanie różnicy między nimi lub dodawanie/odejmowanie pewnej ilości czasu do danej daty.

## Zobacz Również

- [Dokumentacja Microsoft o klasie DateTime](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime?view=net-5.0)
- [Przykłady użycia klasy DateTime](https://www.c-sharpcorner.com/UploadFile/mahesh/datetimesamples11232005061825AM/datetimesamples.aspx)
- [Wszystko o pracy z czasem w C#](https://www.tutorialsteacher.com/csharp/csharp-datetime)