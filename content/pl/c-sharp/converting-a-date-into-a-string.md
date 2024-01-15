---
title:                "Zamiana daty na łańcuch znaków"
html_title:           "C#: Zamiana daty na łańcuch znaków"
simple_title:         "Zamiana daty na łańcuch znaków"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest nieodzowna w wielu projektach programistycznych, ponieważ często wymaga się prezentacji daty w czytelnej formie dla użytkownika lub zapisu jej w bazie danych. W tym artykule dowiesz się, jak w prosty sposób dokonać tego w języku C#.

## Jak to zrobić

### Z użyciem metody ToString()

Najprostszym sposobem na przekonwertowanie daty na ciąg znaków jest użycie metody ToString() wbudowanej w klasę DateTime. W przypadku, gdy chcemy uzyskać datę w określonym formacie, należy podać odpowiednie parametry wywołania metody.

```C#
// Utworzenie zmiennej typu DateTime z aktualną datą
DateTime currentDate = DateTime.Now;

// Konwersja daty na ciąg znaków w formacie d/M/yyyy (np. 4/12/2021)
string dateString = currentDate.ToString("d/M/yyyy");
Console.WriteLine(dateString);

// Konwersja daty na ciąg znaków w formacie MMMM d, yyyy (np. April 12, 2021)
string longDateString = currentDate.ToString("MMMM d, yyyy");
Console.WriteLine(longDateString);
```

W powyższym przykładzie używamy metody ToString() z dwoma różnymi formatami daty: "d/M/yyyy" oraz "MMMM d, yyyy". W obu przypadkach rezultat zostanie wyświetlony w postaci ciągu znaków.

### Z zastosowaniem interpolacji

Kolejną opcją jest użycie interpolacji, czyli wprowadzenie zmiennej typu DateTime bezpośrednio w ciąg znaków. W ten sposób możemy dodatkowo dostosować format daty za pomocą specjalnych znaków:

```C#
// Utworzenie zmiennej typu DateTime z aktualną datą
DateTime currentDate = DateTime.Now;

// Konwersja daty na ciąg znaków w formacie d/M/yyyy (np. 4/12/2021)
string dateString = $"{currentDate.Day}/{currentDate.Month}/{currentDate.Year}";
Console.WriteLine(dateString);

// Konwersja daty na ciąg znaków w formacie MMMM d, yyyy (np. April 12, 2021)
string longDateString = $"{currentDate:MMMM d, yyyy}";
Console.WriteLine(longDateString);
```

W powyższym przykładzie używamy składni $"{zmienna:format}", gdzie w miejsce "zmienna" podstawiamy zmienną typu DateTime, a w miejsce "format" podajemy odpowiednią kombinację specjalnych znaków, oznaczających poszczególne części daty.

## Deep Dive

Klasy DateTime i string w języku C# posiadają szereg metod i właściwości, które pozwalają na jeszcze większą kontrolę nad konwersją daty na ciąg znaków. W przypadku bardziej zaawansowanych potrzeb w tej kwestii, warto zapoznać się ze szczegółową dokumentacją tych klas.

## Zobacz także

- Dokumentacja klasy DateTime: https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime?view=net-5.0
- Dokumentacja klasy string: https://docs.microsoft.com/pl-pl/dotnet/api/system.string?view=net-5.0