---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "PowerShell: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Wyszukiwanie i zamienianie tekstu (ang. searching and replacing text) oznacza znajdowanie w tekście określonych fraz i zamienianie ich na inne. Programiści wykonują tę czynność w celu aktualizacji danych lub poprawienia błędów w kodzie.

## Jak:

Informatycy, którzy używają PowerShell, mogą użyć funkcji Replace(), aby wykonać operację wyszukiwania i zamiany tekstu w dokumentach tekstowych. Poniżej znajduje się przykładowy kod, który zamienia wszystkie wystąpienia słowa "hello" na "cześć" w pliku tekstowym.

```PowerShell
$FileContent = Get-Content -Path C:\Users\Example\TextFile.txt
$NewContent = $FileContent -Replace "hello", "cześć"
Set-Content -Path C:\Users\Example\TextFile.txt -Value $NewContent
```

W powyższym przykładzie funkcja Get-Content wczytuje zawartość pliku tekstowego, a funkcja Replace() zamienia wszystkie wystąpienia słowa "hello" na "cześć". Następnie funkcja Set-Content zapisuje zmienioną zawartość do tego samego pliku.

## Dojrzały projekt:

Wyszukiwanie i zamienianie tekstu jest powszechną funkcją w większości języków programowania, w tym w PowerShell. Wcześniej programiści musieli używać różnych narzędzi lub funkcji w celu dokonania tej operacji, jednak dzięki PowerShell jest to możliwe w jednym języku.

## Zobacz także:

Jeśli chcesz dowiedzieć się więcej o funkcji Replace() w języku PowerShell, zajrzyj na stronę dokumentacji Microsoft:
https://docs.microsoft.com/pl-pl/powershell/module/microsoft.powershell.core/about/about_replace?view=powershell-7.1.3