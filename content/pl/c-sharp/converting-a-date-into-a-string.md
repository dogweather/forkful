---
title:                "C#: Konwersja daty do ciągu znaków"
simple_title:         "Konwersja daty do ciągu znaków"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli pracujesz z datami w programowaniu C#, może zdarzyć się, że będziesz musiał przekonwertować datę na ciąg znaków. Możesz zastanawiać się, po co to robić, skoro przecież istnieją specjalne typy dla dat. Cóż, na przykład, jeśli chcesz wyświetlić datę w czytelnej formie dla użytkownika lub zapisać ją do pliku, konwersja na ciąg znaków będzie konieczna.

## Jak to zrobić

Aby przekonwertować datę na ciąg znaków w programie C#, możesz użyć metody `ToString()` z obiektu `DateTime`. Przykładowo, jeśli chcesz wyświetlić obecną datę w formacie "dzień.miesiąc.rok", możesz to zrobić tak:

```C#
DateTime dzisiaj = DateTime.Now;
string data = dzisiaj.ToString("dd.MM.yyyy");
Console.WriteLine(data);
```

Wyjście na ekranie zostanie wyświetlone jako "30.10.2021".

Możesz również podać różne formaty daty w metodzie `ToString()` (np. "dd/MM/yyyy" lub "MM-dd-yyyy"), aby dostosować wyjście do swoich potrzeb. W dokumencie dokumentacji Microsoft możesz znaleźć pełną listę dostępnych formatów.

## Deep Dive

Warto również wspomnieć, że oprócz metody `ToString()`, istnieje również odwrotna metoda `Parse()`, która pozwala na przekonwertowanie ciągu znaków na obiekt `DateTime`. Podobnie jak w przypadku metody `ToString()`, możesz określić format ciągu znaków, aby dopasować go do daty, którą chcesz otrzymać.

W przypadku, gdy masz do czynienia z datami w różnych strefach czasowych, pomocne może być użycie opcji `ToString()` lub `Parse()` z wykorzystaniem obiektu `CultureInfo`, który pozwala na sprecyzowanie preferowanego języka i kultury.

## Zobacz również

- [DateTime.ToString() metoda (Microsoft Docs)](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime.tostring)
- [DateTime.Parse() metoda (Microsoft Docs)](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime.parse)
- [DateTime.ParseExact() metoda (Microsoft Docs)](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime.parseexact)
- [CultureInfo klasa (Microsoft Docs)](https://docs.microsoft.com/pl-pl/dotnet/api/system.globalization.cultureinfo)