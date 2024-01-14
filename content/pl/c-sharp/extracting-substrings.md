---
title:    "C#: Wydobywanie podciągów"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Dlaczego 

Czy kiedykolwiek potrzebowałeś wyodrębnić fragment tekstu z większego ciągu znaków? Może widziałeś tekst w postaci "Imię Nazwisko", ale potrzebowałeś tylko imienia. W takiej sytuacji, możliwość wyodrębnienia podstringów w C# może być niezwykle przydatna. Pozwala ona na pracę z konkretnymi fragmentami tekstu, potrzebnymi do dalszych operacji.

## Jak to zrobić

W C#, wyodrębnianie podstringów jest możliwe dzięki użyciu metody `Substring()`. Metoda ta przyjmuje dwa argumenty - indeks początkowy oraz długość wyodrębnianego podstringa. Przykładowo, jeśli chcemy wyodrębnić tylko imię z ciągu "John Doe", możemy użyć poniższego kodu:

```C#
string fullName = "John Doe";
string firstName = fullName.Substring(0, 4);
Console.WriteLine(firstName); // Output: John
```

W powyższym przykładzie, podstring zaczyna się od indeksu 0 (pierwszej litery) i ma długość 4. Dzięki temu, że w C# indeksy zaczynają się od 0, imię "John" zostaje poprawnie wyodrębnione.

Możliwe jest także wyodrębnienie podstringa od danego indeksu do końca tekstu. Przykładowo, jeśli chcemy pobrać nazwisko z ciągu "John Doe", możemy użyć poniższego kodu:

```C#
string fullName = "John Doe";
string lastName = fullName.Substring(5);
Console.WriteLine(lastName); // Output: Doe
```

W tym przypadku, nie podajemy długości podstringa, co powoduje, że zostanie on wyodrębniony od indeksu 5 (początek nazwiska) do końca tekstu.

## Głębszy wgląd

Metoda `Substring()` jest przydatna nie tylko w wyodrębnianiu podstringów z określonych pozycji. Możliwe jest także wyodrębnianie fragmentów na podstawie innych kryteriów, takich jak znak lub ciąg znaków. W celu tego, możemy użyć metody `IndexOf()`, która zwraca indeks pierwszego wystąpienia danego znaku lub ciągu znaków. Następnie, możemy wykorzystać ten indeks jako argument dla metody `Substring()`, aby wyodrębnić odpowiedni fragment.

```C#
string fullName = "John Doe";
int spaceIndex = fullName.IndexOf(" "); // znajduje indeks spacji
string firstName = fullName.Substring(0, spaceIndex);
Console.WriteLine(firstName); // Output: John
```

W powyższym przykładzie, najpierw znajdujemy indeks spacji w ciągu "John Doe". Następnie, korzystając z tego indeksu, wyodrębniamy podstring zaczynający się od indeksu 0 i kończący się na indeksie spacji. Dzięki temu, uzyskujemy tylko imię.

## Zobacz także

- Dokumentacja Microsoft: [Substring method](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
- Poradnik C# dla początkujących: [Pracy z tekstem](https://docs.microsoft.com/pl-pl/dotnet/csharp/programming-guide/strings/)