---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "PowerShell: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Wysłanie żądania HTTP z podstawowym uwierzytelnieniem to przysłowiowa kaczka w situacjach, gdy potrzebujemy autoryzować nasze zapytanie do serwera. Programiści często używają tego narzędzia, aby odwzorować interakcje z serwerem w ich skrypcie PowerShell.

## Jak to zrobić?

```PowerShell
Invoke-RestMethod -URI "https://example.com/api/users" -Method Get -Authentication Basic -Credential (Get-Credential)
```

Kod ten wysyła żądanie GET na adres URL "https://example.com/api/users" z wykorzystaniem podstawowego uwierzytelnienia. Następnie poprosi nas o podanie danych uwierzytelniających, których użyjemy do autoryzacji zapytania.

### Przykładowy wynik

```
Status: 200 OK
Body:
{
  "id": "123",
  "name": "John",
  "age": 30 
}
```

Widzimy, że nasze żądanie zostało wykonane pomyślnie i otrzymaliśmy odpowiedź od serwera.

## Głębsze wniknięcie

### Kontekst historyczny

Podstawowe uwierzytelnienie zostało wprowadzone w 1999 roku, jako sposób na autoryzację żądań HTTP. Jest to jedna z najprostszych i najpopularniejszych metod autoryzacji w świecie informatyki.

### Alternatywy

Istnieje wiele innych metod uwierzytelniania, takich jak uwierzytelnianie z wykorzystaniem kluczy API lub tokenów OAuth. Każda z tych metod ma swoje zalety i jest wykorzystywana w różnych scenariuszach. Jednak w przypadku prostych zapytań HTTP, podstawowe uwierzytelnienie jest często wybieraną opcją.

### Szczegóły implementacji

Wysyłając żądanie HTTP z podstawowym uwierzytelnieniem przy użyciu PowerShell, ważne jest, aby upewnić się, że nasze dane uwierzytelniające są prawidłowo sformatowane. Możemy użyć polecenia ```Get-Credential``` do utworzenia poprawnego obiektu Credential, który następnie przekażemy do parametru -Credential w poleceniu ```Invoke-RestMethod```.

## Zobacz również

- [Dokumentacja Microsoft o poleceniu Invoke-RestMethod](https://docs.microsoft.com/pl-pl/powershell/module/Microsoft.PowerShell.Utility/Invoke-RestMethod)
- [Inne metody uwierzytelniania w protokole HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)