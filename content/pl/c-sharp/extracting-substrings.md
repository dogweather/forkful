---
title:                "C#: Wycinanie podciągów"
simple_title:         "Wycinanie podciągów"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego
Często w programowaniu musimy manipulować tym, co nazywamy "łańcuchami znaków" - czyli ciągami liter, cyfr i innych symboli. Czasami potrzebujemy wyodrębnić część tego łańcucha, np. aby uzyskać tylko imię osoby z pełnego adresu e-mail. W takim przypadku przydaje się funkcja "SubString", która pozwala na wybranie określonej części łańcucha, oraz obliczenie jego długości. W tym artykule dowiesz się, jak wykorzystać tę funkcję w języku C#.

## Jak to zrobić
Aby skorzystać z funkcji "SubString" w C#, musimy najpierw utworzyć zmienną typu "string", czyli łańcucha znaków. Będziemy też potrzebowali zmiennej oznaczającej pozycję, od której chcemy wyodrębnić łańcuch, oraz zmiennej określającej długość tego fragmentu. Poniżej przedstawiam przykładowy kod, wykorzystujący funkcję "SubString" do wyodrębnienia imienia ze stringa zawierającego imię i nazwisko:

```C#
string imieNazwisko = "Jan Kowalski";

string imie = imieNazwisko.SubString(0, 3); //w tym przypadku wybieramy pierwsze 3 znaki

Console.Write(imie);
```

Ten kod wyświetli nam na ekranie "Jan", ponieważ funkcja "SubString" zwraca fragment łańcucha od podanej pozycji (pierwszy parametr), o określonej długości (drugi parametr). Jeśli chcesz wybierać fragmenty łańcucha w oparciu o inne warunki, zapoznaj się z dokumentacją dotyczącą funkcji "SubString" w języku C#.

Możesz również wykorzystać funkcję "SubString" do obliczenia długości wybranego fragmentu. W tym celu należy przypisać wynik funkcji do zmiennej typu "int", np:

```C#
string imieNazwisko = "Jan Kowalski";

int dlugoscImienia = imieNazwisko.SubString(0, 3).Length;

Console.Write(dlugoscImienia); //wyświetli wartość 3
```

## Deep Dive
Funkcja "SubString" w języku C# jest bardzo pomocna, gdy potrzebujemy wyodrębnić część łańcucha znaków. Jednak warto pamiętać, że działanie tej funkcji może być różne, w zależności od wykorzystywanej implementacji języka. W przypadku C#, funkcja "SubString" wybiera fragment łańcucha, licząc od pozycji 0. Inne języki mogą działać w inny sposób, dlatego warto dokładnie zapoznać się z dokumentacją podczas korzystania z tej funkcji w różnych językach.

## Zobacz również
- Dokumentacja funkcji "SubString" w języku C# (https://docs.microsoft.com/pl-pl/dotnet/api/system.string.substring?view=net-5.0)
- Inne funkcje służące do manipulacji łańcuchami znaków w języku C# (https://docs.microsoft.com/pl-pl/dotnet/api/system.string?view=net-5.0)