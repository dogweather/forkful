---
title:                "Otrzymywanie aktualnej daty"
html_title:           "C#: Otrzymywanie aktualnej daty"
simple_title:         "Otrzymywanie aktualnej daty"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Co i dlaczego?

Aktualna data jest po prostu dzisiejszą datą, czyli dniem, miesiącem i rokiem, w którym aktualnie się znajdujemy. Programiści potrzebują dostępu do aktualnej daty w celu wykonywania różnych zadań, takich jak śledzenie czasu lub wyświetlanie daty i godziny w aplikacji.

# Jak to zrobić:

Możemy użyć metody `DateTime.Now`, aby uzyskać aktualną datę i czas w formacie standardowych obiektów `DateTime`.

```C#
 DateTime dzis = DateTime.Now;
Console.WriteLine("Dzisiejsza data to: {0}", dzis.Date);
Console.WriteLine("Aktualny czas to: {0:t}", dzis.TimeOfDay);
// Output:
// Dzisiejsza data to: 7/5/2021
// Aktualny czas to: 12:00:00 AM
```

# Głębszy przegląd:

## Kontekst historyczny:

Pierwsza implementacja funkcji pobierania aktualnej daty pojawiła się w języku FORTRAN w roku 1958. W językach programowania takich jak C# dostępna jest wbudowana klasa `DateTime`, która umożliwia łatwe uzyskanie aktualnej daty.

## Alternatywy:

Poza użyciem wbudowanej klasy `DateTime` w C#, istnieją również inne biblioteki i narzędzia, które umożliwiają dostęp do aktualnej daty, takie jak `System.Time` czy `NodaTime`.

## Szczegóły implementacji:

Metoda `DateTime.Now` korzysta z zegara systemowego w celu uzyskania aktualnego czasu. Jest to dokładniejsza metoda niż korzystanie z `DateTime.Today`, która zwraca tylko datę bez uwzględniania czasu.

# Zobacz także:

- [Dokumentacja Microsoft na temat klasy DateTime](https://docs.microsoft.com/pl-d##otnet/api/system.datetime?view=net-5.0)
- [Wprowadzenie do biblioteki NodaTime](https://nodatime.org/1.4.x/userguide/)