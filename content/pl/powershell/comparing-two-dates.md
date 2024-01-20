---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Porównywanie dwóch dat to proces sprawdzania, która data jest wcześniejsza, późniejsza, czy może one są takie same. Programiści robią to, aby manipulować danymi czasowymi, na przykład do sortowania wydarzeń w kolejności chronologicznej.

## Jak to zrobić:

```PowerShell
# Tworzenie dwóch dat
$data1 = Get-Date -Year 2021 -Month 5 -Day 10
$data2 = Get-Date -Year 2021 -Month 5 -Day 15

# Porównywanie dwóch dat
if ($data1 -gt $data2) {
    echo "Data1 jest późniejsza niż Data2"
} elseif ($data1 -lt $data2) {
    echo "Data1 jest wcześniejsza niż Data2"
} else {
    echo "Data1 i Data2 są takie same"
}
```

Ten kod wypisuje na ekranie informacje o różnicy czasowej między dwoma datami.

## W głąb tematu:

1. Kontekst historyczny: W PowerShell, porównywanie dwóch dat było zawsze proste i wydajne, głównie dzięki obiektom System.DateTime .NET.
2. Alternatywy: Można również zastosować metody obiektu .NET jak "CompareTo()" lub "Equals()" do porównywania dat.
3. Szczegóły implementacji: Porównując dwie daty w PowerShell, porównywane są nanosekundy od początku ery, która zaczyna się 1 stycznia 0001 roku.

## Zrozumieć lepiej:

Jeśli chcesz zgłębić temat, oto kilka linków do dodatkowych materiałów:
- Porównywanie dat w PowerShell: https://ss64.com/ps/syntax-compare.html
- Sposoby na manipulację datami w PowerShell: https://docs.microsoft.com/pl-pl/powershell/scripting/samples/working-with-dates-and-times?view=powershell-7.1
- Metody porównywania obiektów .NET: https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime.compare?view=net-5.0