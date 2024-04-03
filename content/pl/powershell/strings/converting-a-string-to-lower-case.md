---
date: 2024-01-20 17:39:15.254339-07:00
description: "How to: Aby przekszta\u0142ci\u0107 string do ma\u0142ych liter w PowerShell,\
  \ u\u017Cywamy metody `.ToLower()`. Oto jak to dzia\u0142a."
lastmod: '2024-03-13T22:44:35.614810-06:00'
model: gpt-4-1106-preview
summary: "Aby przekszta\u0142ci\u0107 string do ma\u0142ych liter w PowerShell, u\u017C\
  ywamy metody `.ToLower()`."
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
weight: 4
---

## How to:
Aby przekształcić string do małych liter w PowerShell, używamy metody `.ToLower()`. Oto jak to działa:

```PowerShell
$exampleString = "PowerShell Jest Super!"
$lowerCaseString = $exampleString.ToLower()
$lowerCaseString
```

Wynik powyższego kodu:

```
powershell jest super!
```

## Deep Dive
Konwersja do małych liter jest standardowym narzędziem w wielu językach programowania, mającym swoje korzenie w starych systemach, gdzie wielkość liter była niekiedy ignorowana. W PowerShell, metoda `.ToLower()` jest częścią typu `String` w .NET Framework, który PowerShell intensywnie wykorzystuje.

Alternatywną metodą może być użycie operatora `-c` wraz z funkcją `Replace`, kiedy chcemy ignorować wielkość liter przy zamianie tekstów.

Oto przykład, jak to można zrealizować:

```PowerShell
$exampleString = "PowerShell JEST Super!"
$exampleString -creplace 'JEST', 'jest'
```

Wynik:
```
PowerShell jest Super!
```

Operator `-c` jest ważny, ponieważ w przeciwnym razie PowerShell traktuje tekst jako wyrażenie regularne. Warto też pamiętać, że metoda `.ToLower()` nie wpływa na znaki spoza alfabetu angielskiego, co może być istotne w kontekście polskich znaków diakrytycznych.

## See Also
Dla bardziej szczegółowych informacji o pracy z tekstami w PowerShell, zobacz:
- Oficjalna dokumentacja PowerShell `.ToLower()` metody: [PowerShell String Methods](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-6.0)
