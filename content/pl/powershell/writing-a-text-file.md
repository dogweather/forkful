---
title:                "Zapisywanie pliku tekstowego"
date:                  2024-01-19
simple_title:         "Zapisywanie pliku tekstowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Zapisywanie pliku tekstowego to po prostu zapisywanie danych w formie czytelnej dla człowieka na dysku. Programiści robią to dla przechowywania konfiguracji, logowania zdarzeń lub wymiany danych z innymi programami.

## How to:
Zapisz tekst do pliku:
```PowerShell
$text = "Cześć, to jest test!"
Set-Content -Path "C:\temp\mojPlik.txt" -Value $text
```

Dodaj tekst do istniejącego pliku:
```PowerShell
Add-Content -Path "C:\temp\mojPlik.txt" -Value "Dodajemy nową linię tekstu."
```

Zapisz tablicę jako wiersze w pliku:
```PowerShell
$linie = "Pierwsza linia", "Druga linia", "Trzecia linia"
$linie | Out-File -FilePath "C:\temp\mojPlik.txt"
```

Oto jak tekst pojawi się w pliku `C:\temp\mojPlik.txt`:
```
Cześć, to jest test!
Dodajemy nową linię tekstu.
Pierwsza linia
Druga linia
Trzecia linia
```

## Deep Dive
Historia: PowerShell wprowadził uproszczenie automatyzacji zadań w Windows, w tym zapisywanie plików tekstowych, co było możliwe wcześniej za pomocą notatnika i skryptów bat.
Alternatywy: Oprócz `Set-Content` i `Add-Content`, można używać `[System.IO.File]::WriteAllText()` lub `[System.IO.File]::AppendAllText()`, które są metodami .NET Framework.
Szczegóły: `Set-Content` nadpisuje zawartość, a `Add-Content` dodaje do istniejącego pliku. `Out-File` ma dodatkowe opcje, jak `-Encoding` do wyboru kodowania pliku.

## See Also
- Oficjalna dokumentacja PowerShell od Microsoft: https://docs.microsoft.com/powershell/
- Praca z plikami i folderami w PowerShell: https://docs.microsoft.com/powershell/scripting/samples/working-with-files-and-folders?view=powershell-7.1
- Poradnik Microsoft do obsługi ciągów tekstowych: https://docs.microsoft.com/powershell/scripting/learn/deep-dives/everything-about-strings?view=powershell-7.1
