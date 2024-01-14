---
title:    "C#: Konkatenacja ciągów znaków"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Dlaczego
Czy kiedykolwiek zastanawiałeś się, dlaczego w C# korzystamy z operacji łączenia stringów? Może wydaje się to oczywiste, ale warto poznać dokładniej mechanizm działania tego procesu.

## Jak to zrobić
```C#
string firstName = "Anna";
string lastName = " Kowalska";
string fullName = firstName + lastName;

Console.WriteLine(fullName);

/* Output: Anna Kowalska */
```
Łączenie stringów jest jedną z podstawowych operacji, które możemy wykonywać w języku C#. Jest to prosta metoda łączenia dwóch lub więcej stringów w jeden, co jest nie tylko wygodne, ale także często niezbędne w programowaniu.

### Wykorzystanie operatora "+" do konkatenacji
Pierwszym sposobem na łączenie stringów jest wykorzystanie operatora "+" między dwoma stringami. 
```C#
string hello = "Cześć";
string name = "Ania";
string greeting = hello + " " + name;

Console.WriteLine(greeting);

/* Output: Cześć Ania */
```
Operator "+" działa podobnie jak w przypadku dodawania liczb, ale w taki sposób łączy ze sobą ciągi tekstowe. Kolejność nie ma znaczenia, możemy łączyć stringi z udziałem innych zmiennych.

### Metoda Concat()
Innym sposobem na łączenie stringów jest użycie metody Concat(). Jest to funkcja wbudowana w język C#, która przyjmuje pojedynczy lub wiele argumentów i zwraca połączony string.
```C#
string part1 = "Cześć";
string part2 = "Ania";

string greeting = string.Concat(part1, " ", part2);
Console.WriteLine(greeting);

/* Output: Cześć Ania */
```
Metoda Concat() może być przydatna, gdy chcemy połączyć więcej niż dwa stringi lub gdy mamy już przygotowane fragmenty tekstu, które chcemy połączyć.

### StringBuilder
Jeśli w programie potrzebujemy często łączyć stringi, zamiast wykorzystywać operator "+" lub metodę Concat(), warto skorzystać z klasy StringBuilder. Budowanie stringu przy użyciu tej klasy jest bardziej wydajne, ponieważ nie tworzy ona nowego obiektu przy każdym złączeniu, tylko modyfikuje już istniejący.
```C#
StringBuilder sb = new StringBuilder("Cześć");
sb.Append(" Ania");
sb.Append("!");
string greeting = sb.ToString();
Console.WriteLine(greeting);

/* Output: Cześć Ania! */
```

## Głębszy zanurzenie
W języku C# stringi są traktowane jako niemodyfikowalne, co oznacza, że po ich utworzeniu nie można zmienić wartości pojedynczych znaków. Dlatego też przy łączeniu stringów tworzone są nowe obiekty zawierające połączone wartości. W przypadku większej ilości łączeń może to wpłynąć na wydajność naszego programu.

Dlatego też, gdy wymagamy wydajnego łączenia stringów, warto skorzystać z klasy StringBuilder, która nie tworzy nowych obiektów przy każdym złączeniu.

## Zobacz także
- [Dokumentacja C# - konkatenacja stringów](https://docs.microsoft.com/pl-pl/dotnet/csharp/whats-new/csharp-6#string-interpolation)
- [Podstawowe operacje na stringach w C#](https://devstyle.pl/2019/03/04/metody-i-operatory-stringow-csharp/)
- [Porównanie wydajności konkatenacji stringów w C#](https://en.it1352.com/article/fb72035c453e40a08ad4e4efc4ccece6.html)