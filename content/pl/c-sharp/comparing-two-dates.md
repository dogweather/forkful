---
title:    "C#: Porównywanie dwóch dat"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Dlaczego 

Dlaczego porównywanie dat jest ważne w programowaniu? Porównywanie dat jest niezbędne w sytuacjach, gdy chcemy sprawdzić, czy dwa wydarzenia miały miejsce w tym samym czasie lub aby zdecydować, czy pewien czas jest później czy wcześniej od innego.

## Jak to zrobić 

Porównywanie dat w C# jest proste i można to zrobić na wiele sposobów. Możesz użyć wbudowanych metod, takich jak `DateTime.Equals()` lub `DateTime.Compare()`, lub też możesz napisać własną funkcję porównującą. Przykładowy kod w języku C# wyglądałby następująco:

```C#
DateTime data1 = new DateTime(2021, 03, 27);
DateTime data2 = new DateTime(2020, 12, 06);

// porównanie dwóch dat przy użyciu metody Equals()
if (data1.Equals(data2))
{
	Console.WriteLine("Daty są takie same.");
}
else
{
	Console.WriteLine("Daty są różne.");
}

// porównanie dwóch dat przy użyciu metody Compare()
int compare = DateTime.Compare(data1, data2);

if (compare < 0)
{
	Console.WriteLine("Data 1 jest wcześniejsza od daty 2.");
}
else if (compare > 0)
{
	Console.WriteLine("Data 1 jest późniejsza od daty 2.");
}
else
{
	Console.WriteLine("Daty są takie same.");
}

// własna funkcja porównująca daty
bool porownajDaty(DateTime data1, DateTime data2)
{
	if (data1.Year > data2.Year)
	{
		return true;
	}
	else if (data1.Year == data2.Year)
	{
		if (data1.Month > data2.Month)
		{
			return true;
		}
		else if (data1.Month == data2.Month)
		{
			if (data1.Day > data2.Day)
			{
				return true;
			}
		}
	}

	return false;
}

// wywołanie własnej funkcji porównującej
if (porownajDaty(data1, data2))
{
	Console.WriteLine("Data 1 jest późniejsza od daty 2.");
}
else
{
	Console.WriteLine("Data 1 jest wcześniejsza od daty 2.");
}
```

W przypadku porównywania dat, ważne jest również pamiętanie o poprawnym formatowaniu, aby zapobiec błędom.

## Głębszy wgląd 

Podczas porównywania dat istnieje wiele czynników, które mogą wpłynąć na wyniki, takich jak strefy czasowe, wieloletnie cykle kalendarzowe czy czas letni/czas zimowy. W C# są dostępne różne funkcje i metody, które pozwalają na uwzględnienie tych czynników i dokładne porównanie dat.

## Zobacz również

* [Dokumentacja C# - DateTime.Equals()](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime.equals)
* [Dokumentacja C# - DateTime.Compare()](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime.compare)
* [Porównywanie dat i godzin w C#](https://www.c-sharpcorner.com/blogs/comparing-dates-and-times-in-c-sharp-coders-hub)