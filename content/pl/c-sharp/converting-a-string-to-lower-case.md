---
title:    "C#: Konwersja ciągu znaków na małe litery"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja tekstu na małe litery jest częstym zadaniem w programowaniu, szczególnie w przypadku tworzenia aplikacji internetowych czy przetwarzania danych. Dzięki temu możemy porównywać teksty bez względu na wielkość liter oraz poprawić czytelność kodu. W tym artykule dowiesz się jak dokonać konwersji w języku C#.

## Jak to zrobić

W języku C# istnieje wbudowana metoda `ToLower()` która przyjmuje jako argument tekst i zwraca jego wersję złożoną z małych liter. Poniżej znajdują się przykładowe kody ilustrujące jej użycie:

```C#
// Przykład 1
string tekst = "PROGRAMOWANIE JEST SUPER!";
string wynik = tekst.ToLower();

Console.WriteLine(wynik); // "programowanie jest super!"

// Przykład 2
string nazwa = "John Smith";
string wynik = nazwa.ToLower();

Console.WriteLine(wynik); // "john smith"
```

### Obiekty typu `String`

W języku C# klasa `String` jest niezmienna, co oznacza, że każde działanie na obiekcie typu `String` zwróci nowy obiekt, a nie zmieni istniejący. Dlatego używając metody `ToLower()` nie zmieniamy oryginalnego tekstu, a jedynie otrzymujemy wynikową wartość.

### Obsługa wyjątków

Należy pamiętać, że metoda `ToLower()` jest niewrażliwa na wyjątki, dlatego warto zawsze sprawdzać czy tekst, który przekazujemy jako argument, nie jest pusty lub czy nie zawiera znaków specjalnych, które mogą powodować problemy w dalszej obróbce. W przypadku błędnego argumentu, metoda zwróci pusty tekst.

## Pogłębiona analiza

Konwersja do małych liter w języku C# jest realizowana przez użycie tablicy z wartościami Unicode. Dzięki temu możemy bez problemu obsługiwać litery diakrytyzowane, takie jak "ą", "ń" czy "ź".

Podczas przetwarzania tekstów w języku angielskim warto zwracać uwagę na różnice w wersalach. Często eksportowane dane mogą zawierać puste spacje lub białe znaki na końcu, co może skutkować niepoprawnym działaniem metody `ToLower()`. W takich przypadkach zaleca się użycie metody `Trim()` przed użyciem `ToLower()`.

## Zobacz także

- [Metoda ToLower() - dokumentacja Microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=netcore-3.1)
- [Inne metody konwersji tekstu w C#](https://www.educba.com/string-toupper-c-sharp/)