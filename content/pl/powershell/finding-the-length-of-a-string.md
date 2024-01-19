---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Określenie długości łańcucha oznacza identyfikację liczby znaków w danym ciągu. Programiści robią to, aby efektywnie zarządzać i manipulować danymi tekstowymi.

## Jak to zrobić:

Używamy cmdlet `Measure-Object`, aby określić długość łańcucha w PowerShell.

```Powershell
$ciagZnakow ="Powershell jest fajny"
$ciagZnakow.Length
```

Zwróci to liczbę 21, co oznacza, że łańcuch zawiera 21 znaków.

## Deep Dive

Historia: Pomiar długości łańcucha nie jest nową koncepcją w programowaniu. Właściwość `Length` w PowerShell istnieje od jego początków.

Alternatywy: Choć `Length` jest najprostszym sposobem znalezienia długości łańcucha, można też użyć cmdlet `Measure-Object`. 

```Powershell
$ciagZnakow | Measure-Object -Character | Select-Object -Property Characters
```

Implementacja: Wewnętrznie, `Length` jest właściwością klasy string w .NET Framework, której PowerShell jest częścią. Działanie polega na zliczaniu znaków łańcucha, co jest efektywne i niezawodne.

## Zobacz też

1. [String.Length Property in .NET](https://docs.microsoft.com/en-us/dotnet/api/system.string.length)
2. [Using the PowerShell "Length" Property](https://www.red-gate.com/simple-talk/sysadmin/powershell/powershell-day-to-day-admin-tasks-length-property/) 
3. [Guide to PowerShell String](https://www.educba.com/powershell-string/)
4. [PowerShell Measure-Object cmdlet](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/measure-object)