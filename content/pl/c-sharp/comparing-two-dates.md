---
title:                "Porównywanie dwóch dat"
html_title:           "C#: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat może być przydatne w wielu przypadkach. Może pomóc w ustalaniu, czy dana data jest przed, po czy też w tym samym dniu co inna data. Może również być wykorzystane w logice biznesowej lub w analizowaniu trendów czasowych.

## Jak to zrobić

Porównywanie dat w języku C# jest bardzo proste i wymaga jedynie wykorzystania kilku wbudowanych funkcji. Poniżej znajdują się przykładowe kody i wyniki dla różnych scenariuszy porównywania dwóch dat.

```C#
// Przykład 1: Porównywanie dat w tym samym dniu

DateTime data1 = new DateTime(2021, 5, 12);
DateTime data2 = new DateTime(2021, 5, 12);

if (data1.Equals(data2)) {
    Console.WriteLine("Daty są identyczne.");
}
// Output: Daty są identyczne.

// Przykład 2: Porównywanie pomiędzy datami przed i po

DateTime data1 = new DateTime(2021, 5, 10);
DateTime data2 = new DateTime(2021, 5, 12);

if (data1 < data2) {
    Console.WriteLine("Data 1 jest wcześniejsza niż data 2.");
} else if (data1 > data2) {
    Console.WriteLine("Data 1 jest późniejsza niż data 2.");
} else {
    Console.WriteLine("Daty są identyczne.");
}
// Output: Data 1 jest wcześniejsza niż data 2.

// Przykład 3: Porównywanie dat z wykorzystaniem DataTime.Compare()

DateTime data1 = new DateTime(2021, 5, 10);
DateTime data2 = new DateTime(2021, 5, 12);

int wynik = DateTime.Compare(data1, data2);

if (wynik < 0) {
    Console.WriteLine("Data 1 jest wcześniejsza niż data 2.");
} else if (wynik > 0) {
    Console.WriteLine("Data 1 jest późniejsza niż data 2.");
} else {
    Console.WriteLine("Daty są identyczne.");
}
// Output: Data 1 jest wcześniejsza niż data 2.
```

## Deep Dive

W języku C# porównywanie dat odbywa się na podstawie wartości obiektów DateTime. W przykładzie 1 wykorzystano metodę Equals(), która porównuje wartości daty wraz z informacją o czasie. W przypadku, gdy zależy nam jedynie na porównaniu dat, można wykorzystać metodę Date.Equals(). Ponadto, można również wykorzystać operatory porównania ">, < lub ==".

W przykładzie 2 wykorzystano operatory porównania, jednak może to stwarzać pewne problemy w przypadku, gdy daty są identyczne, ale różnią się informacją o czasie. Dlatego też lepszym rozwiązaniem jest wykorzystanie metody Compare(), która zwraca wartość ujemną, jeśli pierwsza data jest wcześniejsza, wartość dodatnią, jeśli jest późniejsza lub 0, jeśli daty są identyczne.

## Zobacz także
- [Porównywanie dat w języku C#](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime.compare?view=net-5.0)
- [Wykorzystanie daty w języku C#](https://www.tutorialsteacher.com/csharp/csharp-datetime)
- [Przeciążanie metod Equals() i Compare() w klasie DateTime](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime.equals?view=net-5.0)