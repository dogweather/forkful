---
title:                "Łączenie łańcuchów znaków"
date:                  2024-01-20T17:35:17.521422-07:00
model:                 gpt-4-1106-preview
simple_title:         "Łączenie łańcuchów znaków"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Concatenation to łączenie kilku stringów w jeden. Programiści robią to, by składać tekst z różnych części, np. tworząc wiadomości czy dynamiczne treści.

## How to: (Jak to zrobić:)
```PowerShell
# Using the + operator
$greeting = "Cześć"
$name = "Ania"
$fullGreeting = $greeting + ", " + $name + "!"
Write-Host $fullGreeting # Cześć, Ania!

# Using the -f operator for formatted strings
$template = "Dzień dobry, {0} {1}!"
$firstName = "Jan"
$lastName = "Kowalski"
$formattedGreeting = $template -f $firstName, $lastName
Write-Host $formattedGreeting # Dzień dobry, Jan Kowalski!

# Using the -join operator
$words = "Mam", "na", "imię", "Ewa"
$joinedSentence = $words -join " "
Write-Host $joinedSentence # Mam na imię Ewa
```

## Deep Dive (Głębsze spojrzenie)
Concatenation w PowerShellu jest proste, ale warto znać kontekst. W przeszłości używano operatora `+`, ale może być nieefektywny przy dużej ilości łańcuchów. Operator `-f` jest użyteczny przy formatowaniu i wstawianiu wartości. Metoda `.Join()` lub operator `-join` są świetne do łączenia kolekcji stringów.

Nowsze wersje PowerShell obsługują "here-strings", które pozwalają na tworzenie wieloliniowych stringów i ułatwiają concatenation bez obaw o znaki nowej linii.

Alternatywą jest również użycie StringBuilder z .NET, przydatne do optymalizacji w skryptach, które wykonują wiele operacji łączenia stringów.

## See Also (Zobacz także)
- [About Join](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_join?view=powershell-7)
- [Using -f Operator for Formatting Strings](https://ss64.com/ps/syntax-f-operator.html)
