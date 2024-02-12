---
title:                "Korzystanie z wyrażeń regularnych"
aliases:
- /pl/powershell/using-regular-expressions/
date:                  2024-02-03T19:17:50.252319-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z wyrażeń regularnych"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyrażenia regularne (regex) to sekwencje znaków formujące wzorzec wyszukiwania, głównie używane do wyszukiwania ciągów znaków i ich manipulacji. Programiści wykorzystują regex w PowerShell do zadań takich jak walidacja danych, parsowanie i transformacja ze względu na jego efektywność i elastyczność w obsłudze złożonych wzorców.

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
