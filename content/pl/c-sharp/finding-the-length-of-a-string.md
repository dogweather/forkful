---
title:    "C#: Znalezienie długości ciągu znaków"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Długość ciągu znaków to ważny aspekt wielu programów i aplikacji. Może ona pomóc nam w określeniu ilości tekstu, który jest wyświetlany, lub ustaleniu, czy użytkownik wprowadził wymaganą ilość znaków, na przykład w formularzach internetowych. W tym artykule dowiesz się, jak znaleźć długość ciągu znaków w języku C#.

## Jak To Zrobić

Aby znaleźć długość ciągu znaków, możemy skorzystać z metody `Length` dostępnej w klasie `String`. Przyjmując za przykład ciąg znaków "Cześć!", możemy zapisać to w następujący sposób:

```C#
string powitanie = "Cześć!";
int dlugosc = powitanie.Length;

Console.WriteLine("Długość ciągu 'Cześć!' to: " + dlugosc);
```

Wynikiem działania tego kodu będzie wyświetlenie na ekranie wartości `6`, ponieważ ciąg "Cześć!" składa się z 6 znaków, wliczając w to również spacje.

Możemy również wprowadzić ciąg znaków poprzez odczytanie go od użytkownika. W tym celu użyjemy metody `ReadLine` w klasie `Console`, a następnie wykorzystamy metodę `Length` do wyświetlenia długości wprowadzonego ciągu:

```C#
Console.WriteLine("Wprowadź dowolny ciąg znaków:");
string ciag = Console.ReadLine();
int dlugosc = ciag.Length;

Console.WriteLine("Długość wprowadzonego ciągu to: " + dlugosc);
```

## Deep Dive

Długość ciągu znaków jest określana na podstawie liczby elementów w tym ciągu, a nie na podstawie znaków. Oznacza to, że spacje, interpunkcja i inne specjalne znaki również są uwzględniane w obliczeniach długości.

Ponadto, długość ciągu może również zostać zmieniona za pomocą metody `Substring`. Pozwala to na wybór konkretnych fragmentów ciągu, co ma wpływ na długość. Na przykład, jeśli wykorzystamy metodę `Substring` na ciągu "Cześć!", wybierając tylko pierwsze 4 znaki, otrzymamy nowy ciąg "Cześ", którego długość wyniesie 4.

## Zobacz Również

- Dokumentacja Microsoft dla metody `Length` w klasie `String`: https://docs.microsoft.com/pl-pl/dotnet/api/system.string.length?view=net-5.0
- Wideo tutorial na temat szukania długości ciągu w języku C#: https://www.youtube.com/watch?v=ro4-gtOmCxQ
- Przykłady użycia metody `Length` w języku C#: https://www.techonthenet.com/csharp/strings/length.php