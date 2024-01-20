---
title:                "Konkatenacja ciągów znaków"
html_title:           "Bash: Konkatenacja ciągów znaków"
simple_title:         "Konkatenacja ciągów znaków"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Łączenie łańcuchów (string concatenation) to proces łączenia dwóch lub więcej łańcuchów string w jeden. Programiści robią to, aby manipulować danymi tekstowymi i stworzyć dynamiczne ciągi znaków.

## Jak to zrobić:
Kodując w języku C# masz kilka możliwości do łączenia stringów. Przykładowe kody:

1. Korzystając z operatora + :
```C#
string przyslowie = "Kto" + " rano" + " wstaje," +" temu" +" Pan Bóg" + " daje.";
Console.WriteLine(przyslowie);
```
**Output:**  *Kto rano wstaje, temu Pan Bóg daje.*

2. Korzystając z funkcji String.Concat():
```C#
string przyslowie = String.Concat("Kto", " rano", " wstaje,", " temu", " Pan Bóg", " daje.");
Console.WriteLine(przyslowie);
```
**Output:** *Kto rano wstaje, temu Pan Bóg daje.*

3. Z użyciem StringBuilder:
```C# 
StringBuilder sb = new StringBuilder();
sb.Append("Kto");
sb.Append(" rano");
sb.Append(" wstaje,");
sb.Append(" temu");
sb.Append(" Pan Bóg");
sb.Append(" daje.");
Console.WriteLine(sb.ToString());
```
**Output:** *Kto rano wstaje, temu Pan Bóg daje.*

## Dogłębne Studium od String Concatenation:

1. Kontekst historyczny: W pierwszych wersjach C# operator + był jedynym sposobem na łączenie stringów. Lecz, był nieefektywny pod względem wydajności dla dużych łańcuchów. Dlatego wprowadzono nowe metody: String.Concat() i StringBuilder.

2. Alternatywy: Oprocz wspomnianych metod, jest wiele innych, jak String.Join() czy String.Format(). Wybór zależy od konkretnej sytuacji i wymagań wydajności.

3. Szczegóły implementacji: Operator + i String.Concat() działają szybko, ale mogą spowodować spadek wydajności dla większych ilości stringów. StringBuilder jest bardziej wydajny dla dużej liczby lub dużych stringów, ponieważ nie tworzy nowego stringu za każdym razem, tylko dodaje do istniejącego.

## Zobacz Również:
* Microsoft Docs o łączeniu stringów: [link](https://docs.microsoft.com/pl-pl/dotnet/csharp/programming-guide/strings/how-to-concatenate-multiple-strings)
* Wydajność String.Concat vs StringBuilder: [link](https://stackoverflow.com/questions/21078/string-concatenation-vs-string-builder-performance)