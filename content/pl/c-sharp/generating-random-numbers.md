---
title:                "Generowanie losowych liczb"
html_title:           "C#: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O co & dlaczego?
Generowanie losowych liczb jest jednym z podstawowych zadań programistycznych. W skrócie, polega ono na tworzeniu sekwencji liczb w sposób losowy. Programiści często wykorzystują ten proces do symulacji przypadkowych zdarzeń lub do zabezpieczenia aplikacji, np. przez generowanie losowych haseł.

## Jak to zrobić:
```C#
// Przykładowy kod generujący losową liczbę całkowitą z przedziału 1-10
Random rnd = new Random();
int randomNumber = rnd.Next(1, 11);
Console.WriteLine(randomNumber);

// Przykładowy kod generujący losowy łańcuch znaków o długości 8
Random rnd = new Random();
const string chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
string randomString = new string(Enumerable.Repeat(chars, 8).Select(s => s[rnd.Next(s.Length)]).ToArray());
Console.WriteLine(randomString);
```
Przykłady te wykorzystują klasę `Random` z przestrzeni nazw `System`, która pozwala na generowanie liczb losowych w programie. Metoda `Next()` pozwala na ustalenie przedziału, z którego ma zostać wylosowana liczba, a metoda `ToArray()` pozwala na przekonwertowanie wygenerowanych znaków na ciąg znaków.

## W głąb:
Generowanie losowych liczb jest ważną częścią programowania od początków tej dziedziny. Jedną z pierwszych metod generowania liczb w sposób losowy była metoda tzw. *randomizer gears*, która wykorzystywała zegary mechaniczne. Obecnie istnieją również inne metody generowania liczb pseudolosowych, które są szybsze i bardziej skuteczne, np. algorytm Mersenne Twister.

Aby zapewnić większe bezpieczeństwo w aplikacjach, programiści wykorzystują również specjalne funkcje skrótu, takie jak SHA-1 lub SHA-2, do generowania losowych liczb. Szyfrowanie tych liczb jest utrudnieniem dla hakera i przeciwdziała złamaniu danych.

## Zobacz również:
- Dokumentacja Microsoft dla klasy `Random`: https://docs.microsoft.com/pl-pl/dotnet/api/system.random?view=net-5.0
- Informacje o algorytmie Mersenne Twister: https://pl.wikipedia.org/wiki/Mersenne_Twister