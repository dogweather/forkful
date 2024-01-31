---
title:                "Wykorzystanie wyrażeń regularnych"
date:                  2024-01-19
html_title:           "Arduino: Wykorzystanie wyrażeń regularnych"
simple_title:         "Wykorzystanie wyrażeń regularnych"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Regularne wyrażenia (regex) to mocne narzędzie do wyszukiwania i manipulacji tekstami. Programiści używają ich do automatyzowania zadań, walidacji danych i przetwarzania tekstu efektywnie.

## Jak to zrobić:

### Sprawdzanie formatu kodu pocztowego:
```PowerShell
$postalCode = '00-000'
if ($postalCode -match '^\d{2}-\d{3}$') {
    "Kod pocztowy jest poprawny."
} else {
    "Błędny format kodu pocztowego."
}
```

### Znalezienie wszystkich adresów email:
```PowerShell
$text = 'jan@kowalski.com, bogus-email, malgorzata@domena.pl'
$emails = [regex]::Matches($text, '\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b')
$emails.Value
```

### Podmiana słów 'kolor' na 'color':
```PowerShell
$text = "Wybierz swój ulubiony kolor: czerwony, zielony czy niebieski."
$fixedText = $text -replace 'kolor', 'color'
$fixedText
```

### Przykładowe wyjścia:
Kod pocztowy jest poprawny.
jan@kowalski.com
malgorzata@domena.pl
Wybierz swój ulubiony color: czerwony, zielony czy niebieski.

## Wnikliwe spojrzenie

Regularne wyrażenia są starym konceptem, zrodziły się w latach 50-tych. Alternatywą może być prostsze przeszukiwanie tekstu, jednak regex oferuje zdecydowanie większe możliwości. Implementacja regex w PowerShell sprawia, że skrypty są bardziej elastyczne, a sam PowerShell wykorzystuje silnik .NET do obsługi regularnych wyrażeń.

## Zobacz także

- [Regular Expressions Quick Start](https://www.regular-expressions.info/quickstart.html)
