---
title:                "Otrzymywanie aktualnej daty"
html_title:           "PowerShell: Otrzymywanie aktualnej daty"
simple_title:         "Otrzymywanie aktualnej daty"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Pobieranie aktualnej daty to podstawowa czynność w programowaniu, która pozwala na uzyskanie informacji o aktualnym dniu, miesiącu, roku i czasie. Programiści wykonują tę czynność, aby dokonywać operacji związanych z datami lub do celów raportowania i rejestracji zdarzeń.

## Jak to zrobić:

```PowerShell
Get-Date
```

Ten prosty polecenie zwróci aktualną datę i czas w formacie domyślnym. Jeśli chcesz uzyskać tylko datę bez czasu, dodaj parametr ```-Format``` i określ odpowiedni format daty, na przykład:

```PowerShell
Get-Date -Format "dd-MM-yyyy"
```

Kod powyżej zwróci aktualną datę w formacie dd-MM-yyyy, czyli dzień-miesiąc-rok. Istnieje wiele różnych formatów, w których możesz wyświetlać datę, więc zawsze możesz dostosować go do swoich potrzeb.

## Głębsze zagłębienie:

W PowerShell pobieranie aktualnej daty jest możliwe dzięki wbudowanej funkcji ```Get-Date```. Jest to polecenie, które pojawiło się w wersji 1.0 i od tego czasu jest niezmiennie obecne w każdej wersji PowerShell.

Alternatywą dla ```Get-Date``` jest użycie metody ```Now``` na obiekcie klasy ```DateTime``` w języku C#, co również zwróci aktualną datę i czas. Jednak użycie wbudowanej funkcji w PowerShell jest zdecydowanie prostszym i szybszym sposobem na uzyskanie aktualnej daty.

Implementacyjnie, funkcja ```Get-Date``` używa dostawcy ```Date and Time``` w PowerShell, który obsługuje interakcję z systemowym zegarem i dostarcza informacji o aktualnej dacie.

## Zobacz także:

[Microsoft Docs - Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7)

[SS64 - Get-Date](https://ss64.com/ps/get-date.html)

[about_DateTime](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/ about_datetime?view=powershell-7)