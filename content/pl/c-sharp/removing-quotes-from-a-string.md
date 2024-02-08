---
title:                "Usuwanie cudzysłowów z ciągu znaków"
aliases:
- pl/c-sharp/removing-quotes-from-a-string.md
date:                  2024-01-26T03:38:44.452190-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usuwanie cudzysłowów z ciągu znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Usuwanie cudzysłowów ze stringa w C# oznacza wyeliminowanie tych uporczywych znaków podwójnego (`"`) lub pojedynczego (`'`) cudzysłowu obejmujących twój tekst. Programiści robią to, aby oczyścić dane, przygotować je do wprowadzenia do bazy danych lub sprawić, żeby stringi były bezpieczne dla dalszego przetwarzania, tak aby nic nie poszło źle, gdy pojawi się przypadkowy cudzysłów.

## Jak to zrobić:
```csharp
string withQuotes = "\"Hello, World!\"";
Console.WriteLine($"Original: {withQuotes}");

// Usuwanie podwójnych cudzysłowów
string withoutDoubleQuotes = withQuotes.Replace("\"", "");
Console.WriteLine($"Bez podwójnych cudzysłowów: {withoutDoubleQuotes}");

// Usuwanie pojedynczych cudzysłowów (zakładając, że twój string je w ogóle zawierał)
string withSingleQuotes = "'Hello, World!'";
string withoutSingleQuotes = withSingleQuotes.Replace("'", "");
Console.WriteLine($"Bez pojedynczych cudzysłowów: {withoutSingleQuotes}");
```

Wynik:
```
Oryginalny: "Hello, World!"
Bez podwójnych cudzysłowów: Hello, World!
Bez pojedynczych cudzysłowów: Hello, World!
```

## Szczegółowa analiza
Koncepcja usuwania cudzysłowów nie jest nowa ani szczególnie skomplikowana, ale jest kluczowa, ponieważ znaki cudzysłowu są często używane do delimitowania stringów. Gdy string zawierający niezamaskowane cudzysłowy zostanie dołączony do bloku kodu lub pliku danych, może to przedwczesnie zakończyć string, powodując błędy lub problemy z bezpieczeństwem, takie jak ataki przez iniekcję.

Historycznie, radzenie sobie z cudzysłowami było częścią procesu walidacji i sanitacji w obsłudze danych. Choć metoda `.Replace()` jest prosta do wyeliminowania cudzysłowów z prostego stringa, w bardziej złożonych scenariuszach, takich jak zagnieżdżone cudzysłowy czy warunkowe usuwanie, mogą być potrzebne bardziej zaawansowane techniki, takie jak wyrażenia regularne.

Alternatywy dla `.Replace()` obejmują metody z klasy `Regex`, gdy potrzebujesz precyzyjnej kontroli lub masz do czynienia z wzorcami, a nie stałymi znakami. Na przykład `Regex.Unescape()` może przydać się przy obchodzeniu się ze znakami ucieczki.

Pod względem implementacji pamiętaj, że stringi w C# są niezmienne, co oznacza, że za każdym razem, gdy używasz `.Replace()`, tworzony jest nowy string. To nie problem przy małych lub jednorazowych operacjach, ale warto mieć to na uwadze pod kątem wydajności przy dużych lub licznych stringach.

## Zobacz także:
- [Dokumentacja metody String.Replace](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netframework-4.8)
- [Wyrażenia regularne w .NET](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- [Najlepsze praktyki bezpiecznej obsługi stringów](https://www.owasp.org/index.php/Data_Validation)
