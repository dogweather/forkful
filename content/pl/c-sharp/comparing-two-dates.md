---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Porównywanie dwóch dat to sprawdzanie, która data jest wcześniejsza, późniejsza lub czy są równe. Programiści robią to na przykład, aby sortować wydarzenia w kolejności chronologicznej.

## Jak to zrobić:

Poniżej znajduje się składnia w języku C# do porównywania dwóch dat. 

Załóżmy, że mamy dwie daty do porównania:

```C#
DateTime date1 = new DateTime(2021, 3, 1);
DateTime date2 = new DateTime(2021, 4, 1);
```

Daty porównujemy za pomocą operatorów standardowych (`<`, `<=`, `>` , `>=` , `==` , `!=`).

```C# 
if(date1 < date2)
{
    Console.WriteLine("date1 jest wcześniejsza niż date2.");
}
else if(date1 > date2)
{
    Console.WriteLine("date1 jest późniejsza niż date2.");
}
else
{
    Console.WriteLine("date1 i date2 są równe.");
}
```

## Głębszy wgląd:

Porównywanie dat jest podstawowym konceptem, który istnieje od dawna w językach programowania. Historycznie, było to nieco bardziej skomplikowane, ale w dzisiejszych językach, takich jak C#, jest to proste i bezpośrednie. 

Jest wiele sposobów porównywania dat w C#. Poza bezpośrednim używaniem operatorów porównania, możemy również użyć metody `DateTime.Compare()`, co jest przydatne, jeśli chcemy zwrócić wartość liczbową reprezentującą wynik porównania.

```C#
int result = DateTime.Compare(date1, date2);
```

Podczas porównywania dwóch dat ważne jest pamiętanie o strefach czasowych. Jeżeli nie są uwzględnione, może to prowadzić do błędów.

## Zobacz również:

Możesz znaleźć więcej informacji na temat pracy z danymi `DateTime` w języku C# w oficjalnej dokumentacji Microsoft: [Dokumentacja C# DateTime](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime?view=net-5.0) 

Aby dowiedzieć się więcej na temat zarządzania strefami czasowymi, zobacz [Dokumentacja C# TimeZoneInfo](https://docs.microsoft.com/pl-pl/dotnet/api/system.timezoneinfo?view=net-5.0).