---
title:                "Odkrywanie długości ciągu znaków"
html_title:           "C#: Odkrywanie długości ciągu znaków"
simple_title:         "Odkrywanie długości ciągu znaków"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeden z podstawowych zadań w programowaniu jest manipulowanie ze stringami, czyli ciągami znaków. Aby to zrobić, często musimy znać długość tych stringów. W tym artykule dowiesz się, dlaczego znajomość długości stringa jest ważna i jak ją uzyskać w języku C#.

## Jak to zrobić

C# ma wbudowaną funkcję "Length", która pozwala nam szybko i łatwo uzyskać długość stringa. Możemy użyć jej na różne sposoby, w zależności od naszej potrzeby. Przykładowe zastosowania tej funkcji znajdziesz poniżej:

```C#
string word = "Hello";
int length = word.Length;
Console.WriteLine(length); //Output: 5
```

Możemy również wykorzystać funkcję "Length" do pętli for w celu wykonania określonego działania dla każdego znaku w stringu:

```C#
string sentence = "This is a long sentence.";
for(int i = 0; i < sentence.Length; i++)
{
    Console.WriteLine(sentence[i]); //Output: This is a long sentence.
}
```

Jeśli chcemy znać długość stringa z uwzględnieniem spacji, możemy wykorzystać funkcję "Replace" w połączeniu z funkcją "Length":

```C#
string sentence = "This is a long sentence.";
string newSentence = sentence.Replace(" ", "");
int length = newSentence.Length;
Console.WriteLine(length); //Output: 21
```

## Deep Dive

W języku C#, stringi są traktowane jako tablica znaków, więc długość stringa jest po prostu liczbą elementów w tej tablicy. Warto jednak pamiętać, że funkcja "Length" liczy każdy znak, nawet spacje, przecinki czy cudzysłowy. Jeśli chcemy zignorować te znaki i poznać tylko liczbę wyrazów w stringu, musimy wykorzystać metodę "Split" i dopiero potem policzyć długość tablicy zwróconej przez tę metodę.

## Zobacz także

- [Dokumentacja C# o funkcji Length](https://docs.microsoft.com/pl-pl/dotnet/api/system.string.length?view=net-5.0)
- [Przykłady wykorzystania funkcji Length w C#](https://www.geeksforgeeks.org/c-sharp-string-length-property/)