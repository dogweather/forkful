---
title:                "Pobieranie aktualnej daty"
aliases: - /pl/powershell/getting-the-current-date.md
date:                  2024-02-03T19:10:31.214149-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pobieranie aktualnej daty"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie bieżącej daty w PowerShell polega na uzyskiwaniu aktualnej daty i godziny systemu. Ta operacja jest fundamentalna dla zadań takich jak logowanie, mierzenie czasu operacji, czy podejmowanie decyzji opartych na datach. Programiści używają tej możliwości do śledzenia zdarzeń, planowania zadań oraz obsługi logiki specyficznej dla dat w skryptach i aplikacjach.

## Jak to zrobić:

PowerShell oferuje proste komendy (cmdlets) do pobierania daty i godziny. Cmdlet `Get-Date` to główne narzędzie do tego celu. Może on zwrócić pełną datę i godzinę, którą możesz formatować lub manipulować według swoich potrzeb.

```powershell
# Pobierz bieżącą datę i godzinę
Get-Date
```

**Przykładowy wynik:**

```
Tuesday, September 5, 2023 9:46:02 AM
```

Możesz również formatować wynik, aby wyświetlał tylko potrzebne ci informacje, takie jak tylko data czy tylko godzina.

```powershell
# Pobierz tylko bieżącą datę w określonym formacie
Get-Date -Format "yyyy-MM-dd"
```

**Przykładowy wynik:**

```
2023-09-05
```

```powershell
# Pobierz tylko bieżącą godzinę
Get-Date -Format "HH:mm:ss"
```

**Przykładowy wynik:**

```
09:46:02
```

### Korzystanie z klasy .NET

PowerShell pozwala na bezpośredni dostęp do klas .NET, oferując alternatywny sposób pracy z datami i godzinami.

```powershell
# Korzystanie z klasy .NET DateTime, aby pobrać bieżącą datę i godzinę
[System.DateTime]::Now
```

**Przykładowy wynik:**

```
Tuesday, September 5, 2023 9:46:02 AM
```

Dla czasu UTC:

```powershell
# Korzystanie z klasy .NET DateTime, aby pobrać bieżącą datę i godzinę UTC
[System.DateTime]::UtcNow
```

**Przykładowy wynik:**

```
Tuesday, September 5, 2023 1:46:02 PM
```

Te komendy i klasy zapewniają potężne i elastyczne opcje pracy z datami i godzinami w PowerShell, niezbędne do wielu zadań skryptowych i automatyzacji.
