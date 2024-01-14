---
title:                "C#: Ekstrakcja podciągów"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Często podczas programowania musimy pracować ze stringami, czyli ciągami znaków. Jedną z przydatnych operacji na stringach jest wyciąganie podciągów, czyli fragmentów tekstu. W tym wpisie dowiesz się, dlaczego wyciąganie podciągów może być przydatne i jak to zrobić w języku C#.

## Jak to zrobić

Do wyciągania podciągów w języku C# możemy użyć metody Substring(). Przyjmuje ona dwa parametry: początkowy indeks oraz długość podciągu. Dzięki temu możemy określić, z którego indeksu chcemy zacząć wyciągać podciąg i jakiej długości ma być ten podciąg.

```C#
// Przykładowy string
string name = "Katarzyna";

// Wyciągnięcie podciągu zaczynającego się od indeksu 2 o długości 6
string substring = name.Substring(2, 6);

// Wyświetlenie wyniku
Console.WriteLine(substring);
// Output: tarzyn
```

Możemy także wykorzystać indeksy ujemne, aby wyciągać podciągi od końca stringa.

```C#
// Przykładowy string
string phrase = "C# to jest super język";

// Wyciągnięcie podciągu zaczynającego się od przedostatniego indeksu o długości 8
string substring = phrase.Substring(phrase.Length - 2, 8);

// Wyświetlenie wyniku
Console.WriteLine(substring);
// Output: język
```

Pamiętajmy, że indeksowanie w języku C# zaczyna się od 0, więc jeśli chcemy wyciągnąć pierwszy znak ze stringa, powinniśmy podać indeks 0.

## Deep Dive

Metoda Substring() działa na zasadzie "od-czego-do-czego", czyli podajemy początkowy indeks oraz długość podciągu, który chcemy wyciągnąć. Możemy również użyć tylko jednego parametru - początkowego indeksu, wtedy Substring() wyciągnie podciąg od podanego indeksu do końca stringa. Istnieje także druga wersja metody, która przyjmuje tylko dwa parametry - początkowy indeks oraz zmienną typu boolean, która określa czy chcemy zwrócić podciąg od danego indeksu włącznie czy wyłącznie.

## Zobacz też

- [Dokumentacja metody Substring() w języku C#](https://docs.microsoft.com/pl-pl/dotnet/api/system.string.substring?view=net-5.0)
- [Szybki przewodnik po operacjach na stringach w C#](https://krispin-gs.github.io/strukturydanych/string/cSharp strings.html)