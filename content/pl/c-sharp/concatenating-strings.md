---
title:                "C#: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

C# jest jednym z najpopularniejszych języków programowania wykorzystywanych w branży IT. Jedną z podstawowych operacji, które możemy wykonać w tym języku, jest łączenie (konkatencjonowanie) ciągów znaków, co pozwala nam na tworzenie bardziej skomplikowanych i dynamicznych aplikacji. W tym artykule przyjrzymy się dlaczego warto nauczyć się tej podstawowej operacji oraz jak ją zastosować w praktyce.

## Jak to zrobić

Aby skonkatencjonować ciągi znaków w C#, możemy skorzystać z operatora "+" lub metody "Concat" z klasy "String". Przykładowy kod wyglądałby następująco:

```
string firstName = "John";
string lastName = "Doe";

string fullName = firstName + " " + lastName;
Console.WriteLine(fullName); // Output: John Doe

string otherFullName = String.Concat(firstName, " ", lastName);
Console.WriteLine(otherFullName); // Output: John Doe
```

Po uruchomieniu powyższego kodu, na konsoli zostanie wyświetlony pełny imię i nazwisko "John Doe". Istnieje również możliwość formatowania ciągów znaków za pomocą metody "Format" z klasy "String", która pozwala na dynamiczne podstawianie zmiennych do wybranego formatu. Przykładowy kod może wyglądać tak:

```
string firstName = "John";
string lastName = "Doe";
int age = 30;

string formattedString = String.Format("{0} {1} is {2} years old.", firstName, lastName, age);
Console.WriteLine(formattedString); // Output: John Doe is 30 years old.
```

W powyższym przykładzie zmienna "formattedString" zostaje sformatowana według określonego formatu, w którym "{0}", "{1}" i "{2}" oznaczają kolejne zmienne podane jako argumenty w metodzie "Format".

## Deep Dive

Konkatencjonowanie ciągów znaków jest bardzo przydatną funkcją, zwłaszcza jeśli pracujemy z dużymi i złożonymi tekstami. W przypadku, gdy musimy łączyć wiele ciągów znaków w celu utworzenia jednego, dużego wyrazu, użycie operatora "+" lub metody "Concat" jest znacznie wygodniejsze i bardziej czytelniejsze niż ręczne dodawanie kolejnych znaków.

Warto również zwrócić uwagę na to, że operacja łączenia ciągów znaków jest wykorzystywana przez wiele innych funkcji w C#, takich jak np. pobieranie danych z bazy danych czy obsługa formularzy w aplikacjach internetowych.

## Zobacz też

- [Dokumentacja C# - Łączenie ciągów znaków](https://docs.microsoft.com/pl-pl/dotnet/csharp/programming-guide/strings/#concatenating-strings)
- [Porównanie wydajności łączenia ciągów znaków w C#](https://www.dotnetperls.com/comparison)
- [Przykładowe zadania z wykorzystaniem łączenia ciągów znaków w C#](http://www.c-sharpcorner.com/UploadFile/7177bd/concatenate-strings-and-print-message-using-C-Sharp/)