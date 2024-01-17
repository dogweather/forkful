---
title:                "Tworzenie pliku tekstowego"
html_title:           "PowerShell: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie plików tekstowych to podstawowa umiejętność każdego programisty. Jest to prosta forma zapisu informacji, która może być wykorzystana w wielu różnych celach. Programiści często używają plików tekstowych do przechowywania danych, konfiguracji czy wyświetlania wyników swoich programów.

## Jak to zrobić:

```PowerShell
# Tworzenie nowego pliku tekstowego o nazwie "hello.txt" z wprowadzonym tekstem
New-Item "hello.txt" -ItemType File -Value "Witaj świecie!"

# Dodawanie tekstu do istniejącego pliku tekstowego
Add-Content "hello.txt" "Dodatkowy tekst"

# Odczytywanie zawartości pliku tekstowego
Get-Content "hello.txt"
```

Wynik:
```PowerShell
Witaj świecie!
Dodatkowy tekst
```

## Deep Dive:

Pisanie plików tekstowych jest jednym z najprostszych sposobów na przechowywanie informacji w formie tekstu. Jest to też najczęściej wykorzystywany format do zapisu danych. Pliki tekstowe są łatwe w odczytywaniu przez człowieka i nie wymagają specjalistycznych narzędzi do ich przetworzenia.

Alternatywnym sposobem na przechowywanie informacji jest używanie baz danych, które oferują bardziej zaawansowane funkcje. Jednak pisanie plików tekstowych jest szybsze i prostsze w implementacji.

## Zobacz także:

[PowerShell - Skrypty w praktyce](https://docs.microsoft.com/pl-pl/powershell/scripting/overview?view=powershell-7.1)

[Podstawowe operacje na plikach w PowerShell](https://www.jitbit.com/alexblog/249-how-to-read-and-write-to-files-in-powershell/)

[Pisanie plików tekstowych w Pythonie](https://realpython.com/read-write-files-python/)