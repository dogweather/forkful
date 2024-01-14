---
title:                "C#: Generowanie losowych liczb"
programming_language: "C#"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest nieodłączną częścią wielu programów komputerowych. Pozwala to na symulację przypadkowych zdarzeń oraz zapewnienie różnorodności w działaniu programów. Dodatkowo, losowe liczby są wykorzystywane w celu zabezpieczenia danych i systemów przed atakami.

## Jak to zrobić

W języku C#, generowanie liczb losowych jest bardzo proste. Wystarczy użyć klasy ```Random``` i jej metody ```Next()```. Poniżej przedstawiono przykładowy kod, który generuje 10 losowych liczb całkowitych od 1 do 100:

```C#
Random random = new Random();

for (int i = 0; i < 10; i++)
{
    int number = random.Next(1, 101); // generuje liczbę od 1 do 100
    Console.WriteLine(number);
}
```

Wynik działania tego kodu może wyglądać następująco:

```
68
14
59
90
27
42
5
19
75
83
```

Podobnie, możemy wygenerować losowe liczby zmiennoprzecinkowe korzystając z metody ```NextDouble()```:

```C#
double randomNumber = random.NextDouble(); // generuje liczbę z przedziału [0, 1)

Console.WriteLine(randomNumber); // możliwe wyniki: 0.234, 0.902, 0.077 itp.
```

Można również określić zakres losowanych liczb oraz ich ilość poprzez odpowiednie ustawienie argumentów w metodzie ```Next()```.

## Deep Dive

W języku C#, generowanie liczb losowych opiera się na algorytmie zw. generatora liniowego kongruentnego. Pozwala to na generowanie liczb pseudolosowych, czyli tych, które wydają się być losowe, ale są w rzeczywistości generowane według pewnego ustalonego wzoru. Dlatego ważne jest, aby uważnie dobierać ustawienia generatora losowych liczb, ponieważ niektóre ustawienia mogą powodować powtarzalność wygenerowanych liczb. W przypadku, gdy potrzebujemy naprawdę losowych liczb, warto skorzystać z bibliotek kryptograficznych, takich jak ```RNGCryptoServiceProvider```.

## Zobacz także

- [Dokumentacja klasy Random w języku C#](https://docs.microsoft.com/pl-pl/dotnet/api/system.random?view=net-5.0)
- [Wykorzystanie liczby losowej w grach komputerowych](https://www.scratchapixel.com/lessons/3d-basic-rendering/randomness/using-randomness-in-games) (język angielski)
- [Kryptograficzne generatory liczb losowych w języku C#](https://www.codeproject.com/Articles/10822/A-CRYSTAL-library-based-on-C-for-cryptographic-gen) (język angielski)