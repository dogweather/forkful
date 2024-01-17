---
title:                "Łączenie ciągów znaków"
html_title:           "PowerShell: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Czym jest i dlaczego?

Łączenie ciągów znaków to proces łączenia dwóch lub więcej ciągów w jeden dłuższy ciąg. Programiści często stosują to narzędzie, aby łatwiej i szybciej tworzyć i manipulować tekstem.

## Jak to zrobić?

Użyj operatora + lub polecenia Join-String, aby połączyć dwa ciągi znaków w jeden.

```PowerShell
# Przykład 1: Używanie operatora + 
"Fajna" + "aplikacja" # Output: Fajnaaplikacja

# Przykład 2: Używanie polecenia Join-String
Join-String -Separator " " -String "Fajna", "aplikacja" # Output: Fajna aplikacja
```

Możesz również połączyć więcej niż dwa ciągi i dostosować separator między nimi. Dodatkowo, parametr -Join w poleceniu Write-Output automatycznie łączy argumenty w jeden ciąg.

## Głębsza analiza

Łączenie ciągów znaków jest powszechnie stosowane w programowaniu i ma szerokie zastosowanie w różnych językach programowania. W PowerShell jest to szczególnie przydatne do przetwarzania tekstu i tworzenia wyrażeń regularnych.

Alternatywą dla łączenia ciągów może być użycie polecenia Format-String, które pozwala na formatowanie tekstu z uwzględnieniem zmiennych. Można również użyć polecenia Out-File, aby zapisać łączony ciąg do pliku.

Implementacja łączenia ciągów w PowerShell jest zoptymalizowana pod kątem wydajności, dzięki czemu jest szybka i wydajna w użyciu.

## Zobacz również

Dokumentacja Microsoft na temat używania operatora + w PowerShell: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_operators 

Dokumentacja Microsoft na temat polecenia Join-String: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/join-string 

Przykłady użycia polecenia Format-String i Out-File: https://www.howtogeek.com/tips/how-to-quickly-create-a-text-file-using-the-command-line-or-powershell/