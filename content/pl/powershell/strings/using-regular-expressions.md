---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:50.252319-07:00
description: "Jak u\u017Cywa\u0107: W PowerShell mo\u017Cna u\u017Cywa\u0107 operator\xF3\
  w `-match`, `-replace` i `-split`, mi\u0119dzy innymi, do wykonywania dzia\u0142\
  a\u0144 z wyra\u017Ceniami regularnymi.\u2026"
lastmod: '2024-03-13T22:44:35.617748-06:00'
model: gpt-4-0125-preview
summary: "W PowerShell mo\u017Cna u\u017Cywa\u0107 operator\xF3w `-match`, `-replace`\
  \ i `-split`, mi\u0119dzy innymi, do wykonywania dzia\u0142a\u0144 z wyra\u017C\
  eniami regularnymi."
title: "Korzystanie z wyra\u017Ce\u0144 regularnych"
weight: 11
---

## Jak używać:
W PowerShell można używać operatorów `-match`, `-replace` i `-split`, między innymi, do wykonywania działań z wyrażeniami regularnymi. Przyjrzyjmy się kilku przykładom:

### Używanie `-match` do sprawdzenia, czy ciąg pasuje do wzorca
Ten operator zwraca `$true`, jeśli wzorzec zostanie znaleziony w ciągu, i `$false` w przeciwnym przypadku.

```powershell
"hello world" -match "\w+orld"
# Wynik: True
```

### Ekstrakcja dopasowań
Możesz uzyskać dopasowaną wartość, odwołując się do automatycznej zmiennej `$matches`.

```powershell
if ("I have 100 apples" -match "\d+") {
    "Znaleziono liczbę: " + $matches[0]
}
# Wynik: Znaleziono liczbę: 100
```

### Używanie `-replace` do substytucji
Operator `-replace` zastępuje wszystkie wystąpienia wzorca określonym ciągiem zastępczym.

```powershell
"foo bar baz" -replace "ba[rz]", "qux"
# Wynik: foo qux qux
```

### Dzielenie ciągów za pomocą `-split`
Dzieli ciąg na tablicę podciągów na podstawie wzorca regex.

```powershell
"The quick-brown_fox jumps" -split "[-_ ]"
# Wynik: The quick brown fox jumps
```

### Zaawansowane dopasowywanie wzorców
PowerShell obsługuje również bardziej złożone operacje regex za pośrednictwem klasy `[regex]`, dając dostęp do metod takich jak `Matches()`, `Replace()` i `Split()`.

```powershell
[regex]::Matches("June 24, August 9, Dec 12", "\b[A-Za-z]+\b").Value
# Wynik: June August Dec

[regex]::Replace("100,000", "\B(?=(?:\d{3})+(?!\d))", ",")
# Wynik: 100,000

[regex]::Split("one,two;three four", ",|;| ")
# Wynik: one two three four
```

Te przykłady pokazują moc i wszechstronność wyrażeń regularnych w PowerShell do manipulacji danymi i dopasowywania wzorców. Wykorzystując regex, programiści mogą efektywnie przeprowadzać złożoną obróbkę tekstu.
