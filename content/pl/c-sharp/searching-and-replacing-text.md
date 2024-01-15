---
title:                "Wyszukiwanie i zamienianie tekstu"
html_title:           "C#: Wyszukiwanie i zamienianie tekstu"
simple_title:         "Wyszukiwanie i zamienianie tekstu"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

WprowadzenieDoświadczasz frustracji, kiedy musisz ręcznie edytować wielokrotnie ten sam fragment tekstu w swoim kodzie? Szukanie i zamiana tekstu może Cię uratować od tego powtarzającego się koszmaru! Nie tylko oszczędzi Ci to czas i wysiłek, ale także pomoże uniknąć błędów ludzkich. W tym artykule dowiesz się, jak w prosty sposób wykonać to zadanie za pomocą C#.

Jak to zrobić?
```C#
var text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit.";
//Zakładamy, że chcemy zmienić wyraz "dolor" na "amor"
var newText = text.Replace("dolor", "amor");
Console.WriteLine(newText);
```
Wynik: "Lorem ipsum amor sit amet, consectetur adipiscing elit."

Jedną z najprostszych metod jest użycie metody Replace() na obiekcie typu string. Nie musimy przejmować się szukaniem indeksu, tylko podajemy tekst, który chcemy zastąpić i tekst, na który chcemy go zamienić. Metoda ta zwraca nowy ciąg znaków, więc warto przechować go w zmiennej.

Jeśli chcemy dokonać wielu zmian w jednym ciągu, możemy skorzystać z metody Replace z wyrażeniem regularnym.
```C#
//Zakładamy, że chcemy zmienić wszystkie wystąpienia "m" na "n"
var regex = new Regex("m");
var newText = regex.Replace(text, "n");
Console.WriteLine(newText);
```
Wynik: "Loren ipsun dnolor sit anet, consectetunradipiscing elit."

Tego typu podejście jest szczególnie przydatne, gdy chcemy dokonać zmian w dużych i skomplikowanych ciągach znaków.

## Deep Dive
Do tej pory skupiliśmy się na metodzie Replace, ale C# oferuje także inne opcje do przeprowadzania zmian w tekście.

Jeśli chcemy zmienić tekst w zależności od pewnych warunków, możemy skorzystać z metody Replace z funkcją zwrotną.
```C#
string ReplaceDelegate(Match match)
{
    //Jeśli na pierwszym miejscu znajduje się mała litera, zamieniamy ją na dużą
    if (char.IsLower(match.Value[0]))
        return char.ToUpper(match.Value[0]) + match.Value.Substring(1);
    //Jeśli na pierwszym miejscu znajduje się duża litera, zamieniamy ją na małą
    else
        return char.ToLower(match.Value[0]) + match.Value.Substring(1);
}
//Zakładamy, że chcemy zmienić pierwszą literę wyrazu "dolor" na dużą literę
var regex = new Regex("dolor");
var newText = regex.Replace(text, ReplaceDelegate);
Console.WriteLine(newText);
```
Wynik: "Lorem Ipsum dolor sit amet, consectetur adipiscing elit."

Możemy także używać wyrażeń regularnych do dokładniejszego określenia, które części tekstu mają zostać zmienione.
```C#
//Zakładamy, że chcemy zmienić wszystkie wystąpienia "am" na "amor"
var regex = new Regex("am(?=or)");
var newText = regex.Replace(text, "amor");
Console.WriteLine(newText);
```
Wynik: "Lorem ipsum amor sit amet, consectetur adipiscing elit."

Podsumowując, manipulowanie tekstem w C# jest nie tylko proste, ale także bardzo przydatne. Wykorzystując różne metody i funkcje, możemy wykonać wiele zmian w tekście w sposób skuteczny i szybki.

## Zobacz też
- [Dokumentacja C# dotycząca metod Replace() i Replace(String, String)](https://docs.microsoft.com/pl-pl/dotnet/api/system.string.replace)
- [Poradnik dotyczący wyrażeń regularnych w C#](https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/regular-expressions)