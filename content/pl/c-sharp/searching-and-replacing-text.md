---
title:                "C#: Wyszukiwanie i zamiana tekstu"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie jest nieodłączną częścią naszego życia. Bez względu na to, czy chcesz zostać programistą czy nie, czasem napotykamy sytuacje, w których musimy zmienić pewien fragment tekstu lub wyrażenia w naszym kodzie. W takim przypadku, niezbędne jest umiejętne posługiwanie się narzędziami do wyszukiwania i zastępowania tekstu. W tym artykule dowiesz się jak można to zrobić w języku C#.

## Jak To Zrobić

W języku C# mamy dostęp do różnych metod i funkcji, które pozwalają nam na wygodne i precyzyjne przeszukiwanie i zmienianie tekstu. Poniżej przedstawione są trzy przykłady wykorzystania tych funkcji, wraz ze zrzutem ekranu wyjścia programu.

```C#
// Przykład 1: Wyszukiwanie i zastępowanie pojedynczego wyrażenia
string text = "Witaj, świecie!";
text = text.Replace("świecie", "moje drogie czytelniki");
Console.WriteLine(text);
```

![Przykład 1](https://i.imgur.com/pYZADHy.png)

```C#
// Przykład 2: Wyszukiwanie i zastępowanie z wykorzystaniem wyrażeń regularnych
string text = "Witaj, 1 stycznia 2021!";
text = Regex.Replace(text, @"\d{1,2} \w+ \d{4}", "jutro");
Console.WriteLine(text);
```

![Przykład 2](https://i.imgur.com/cx7x6Fn.png)

```C#
// Przykład 3: Wyszukiwanie i zastępowanie z wykorzystaniem indeksów
string text = "Dzisiaj jest 1 stycznia 2021";
int index = text.IndexOf("1 stycznia");
text = text.Remove(index, "1 stycznia".Length);
text = text.Insert(index, "2 stycznia");
Console.WriteLine(text);
```

![Przykład 3](https://i.imgur.com/fAOjggl.png)

## Deep Dive

W języku C# do wyszukiwania i zastępowania tekstu możemy wykorzystać kilka różnych funkcji, takich jak `Substring`, `IndexOf`, `Regex.Replace` czy `StringBuilder`. Każda z nich ma swoje specyficzne zastosowanie, dlatego warto zapoznać się z dokumentacją i wybrać odpowiednią w danym przypadku.

Ponadto, istnieją również różne sposoby przeszukiwania tekstu, takie jak wykorzystanie wyrażeń regularnych czy indeksów, co pozwala nam na jeszcze większą precyzję w zastępowaniu tekstu.

Takie narzędzia są nieocenione w codziennej pracy programisty i mogą zaoszczędzić nam dużo czasu i wyrzeczeń. Dlatego warto poświęcić trochę czasu na poznanie tych funkcji i wykorzystywanie ich w praktyce.

## Zobacz także

- [Dokumentacja języka C#](https://docs.microsoft.com/pl-pl/dotnet/csharp/)
- [Przewodnik po wyrażeniach regularnych w C#](https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Artykuł o wykorzystaniu funktionu IndexOf w C#](https://www.geeksforgeeks.org/indexof-method-in-c-sharp/)