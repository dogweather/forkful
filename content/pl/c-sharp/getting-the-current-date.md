---
title:    "C#: Uzyskiwanie bieżącej daty"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Dlaczego warto poznać aktualną datę w programowaniu?

Wiele aplikacji i systemów informatycznych wymaga dostępu do aktualnej daty i czasu. Jest to ważne nie tylko w celu wyświetlania bieżącej daty, ale także do obliczeń daty w przyszłości lub przeszłości. Dlatego też poznanie sposobu uzyskiwania aktualnej daty w programowaniu jest niezbędne dla każdego programisty.

## Jak to zrobić?

W języku C# istnieje kilka sposóbów na uzyskanie aktualnej daty. Możemy skorzystać z klasy `DateTime` i jej metod, takich jak `Now()` czy `Today()`, aby uzyskać datę i czas w bieżącej strefie czasowej. Możemy również wykorzystać klasę `DateTimeOffset`, która pozwala nam na dostęp do daty i czasu w różnych strefach czasowych. Poniżej przedstawiamy przykładowy kod wraz z oczekiwanym wynikiem:

```C#
// uzyskanie aktualnego czasu
DateTime now = DateTime.Now;
Console.WriteLine(now);

// uzyskanie aktualnej daty
DateTime today = DateTime.Today;
Console.WriteLine(today);

// uzyskanie daty z innego regionu czasowego
DateTimeOffset date = new DateTimeOffset(2021, 01, 01, 12, 00, 00, new TimeSpan(2, 0, 0));
Console.WriteLine(date);
```

**Wynik:**

```
3/15/2021 11:30:00 AM
3/15/2021 12:00:00 AM
1/1/2021 12:00:00 PM +02:00
```

Innym sposobem na uzyskanie aktualnej daty jest skorzystanie z klasy `DateTime.Now.ToString()`, która pozwala nam na formatowanie daty i czasu w różny sposób, na przykład:

```C#
// uzyskanie daty w formie długiej
DateTime now = DateTime.Now;
string longDate = now.ToString("D");
Console.WriteLine(longDate);

// uzyskanie daty w formie krótkiej
string shortDate = now.ToString("d");
Console.WriteLine(shortDate);

// uzyskanie godziny
string time = now.ToString("T");
Console.WriteLine(time);
```

**Wynik:**

```
Monday, March 15, 2021
3/15/2021
11:30:00 AM
```

## Głębsze spojrzenie

Klasy `DateTime` i `DateTimeOffset` oferują wiele różnych metod i właściwości, które możemy wykorzystać do manipulowania datami i czasem. Na przykład, możemy wykorzystać metodę `Add()` do dodawania lub odejmowania określonej ilości czasu do daty, takiej jak dni, godziny, minuty lub sekundy. Możemy również wykorzystać metodę `Parse()` lub `TryParse()` do konwersji daty w stringu na obiekt `DateTime`, co może być przydatne, gdy otrzymujemy dane z bazy danych lub zewnętrznego źródła.

# Zobacz również

- [Microsoft Docs: DateTime Struct (C#)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [C# DateTime - GeeksforGeeks](https://www.geeksforgeeks.org/c-sharp-datetime-format-overview-with-examples/)

Nauka uzyskiwania aktualnej daty w programowaniu jest nie tylko przydatna, ale i niezbędna w wielu przypadkach. Dzięki zrozumieniu różnych metod i możliwości klasy `DateTime`, możemy lepiej manipulować datami i czasem w naszych aplikacjach. W ten sposób nasze programy będą jeszcze bardziej precyzyjne i użyteczne dla użytkowników.