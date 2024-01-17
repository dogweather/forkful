---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "C#: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Odnajdywanie długości ciągu znaków jest powszechnym zadaniem programistów. Polega to na obliczeniu liczby znaków w podanym ciągu oraz zwróceniu wyniku. Zadanie to jest często wykorzystywane do analizy tekstu, filtrowania danych lub walidacji wprowadzonych informacji.

## W jaki sposób:

```C#
string text = "Cześć, jestem artykułem o programowaniu!";
int length = text.Length;
Console.WriteLine("Długość tekstu to: " + length);
```
Sample output: Długość tekstu to: 35

Podajemy nasz tekst do zmiennej "text", następnie wykorzystujemy metodę Length, aby obliczyć długość tego tekstu. Wreszcie, korzystając z metody WriteLine, wyświetlamy wynik w konsoli.

## Głębsze zagłębienie:

Historia odnajdywania długości ciągu znaków jest powiązana z rozwojem programowania. Wcześniej programiści musieli samodzielnie obliczać długość tekstu, natomiast dziś jest to funkcja wbudowana w większości języków programowania, w tym w C#. Alternatywnie, możemy również użyć metody GetByteCount, która zwraca liczbę bajtów w podanym ciągu.

## Zobacz również:

- [Documentacja C# dotycząca metody Length](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=net-5.0)
- [Porównanie metod Length i GetByteCount](https://stackoverflow.com/questions/9864526/c-sharp-string-length-getbytecount-getcharcount)