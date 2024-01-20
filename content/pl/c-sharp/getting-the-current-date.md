---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pobieranie Aktualnej Daty w C# 

## Co to jest i dlaczego?

Pobieranie aktualnej daty to proces, w którym program komputerowy odczytuje datę i czas z systemu operacyjnego. Programiści robią to, aby zapisywać daty zdarzeń, logi, czy tworzyć funkcjonalności zależne od czasu.

## Jak to zrobić:

Aby uzyskać bieżącą datę w C#, wystarczy wywołać ```DateTime.Now``` . Oto jak to zrobić:

```C#
DateTime currentDateTime = DateTime.Now;
Console.WriteLine($"Aktualna data i czas: {currentDateTime}");
```
Wyjście tego kodu to:
```C#
Aktualna data i czas: 2022-03-21 17:45:23
```

## W Głąb Tematu

Pobieranie aktualnej daty to jeden z najstarszych elementów programowania i istniało prawie od początku istnienia komputerów. Historycznie, zwykle jest to zrobione poprzez API systemu operacyjnego.

Alternatywą dla pobierania bieżącej daty w C# jest używanie ```DateTime.UtcNow``` . Ta metoda zwraca uniwersalny czas koordynowany (UTC), który jest niezależny od strefy czasowej.

Podczas gdy ```DateTime.Now``` zwraca datę i czas lokalne dla strefy czasowej systemu, ```DateTime.UtcNow``` zwraca datę i czas UTC. Oto jak to zrobić:

```C#
DateTime currentDateTimeUtc = DateTime.UtcNow;
Console.WriteLine($"Aktualna data i czas UTC: {currentDateTimeUtc}");
```

Pod względem implementacji, ```DateTime.Now``` i ```DateTime.UtcNow``` odwołują się do zegara systemu operacyjnego, aby uzyskać aktualną datę i czas.

## Zobacz Też:

- Microsoft Documentation: [System.DateTime (Microsoft Docs)](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime?view=net-6.0)
- Stack Overflow: [“How to get the current date and time in C#?” (Stack Overflow)](https://stackoverflow.com/questions/453161/best-method-to-get-date-of-last-week)