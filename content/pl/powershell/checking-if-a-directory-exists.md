---
title:                "Sprawdzanie, czy katalog istnieje"
date:                  2024-01-20T14:57:53.539442-07:00
html_title:           "Fish Shell: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Sprawdzanie, czy katalog istnieje, to proces weryfikacji, czy określona ścieżka w systemie plików prowadzi do katalogu. Programiści robią to, aby zapobiec błędom podczas próby dostępu lub modyfikacji folderów, które nie istnieją.

## Jak to zrobić:
Sprawdzenie, czy katalog istnieje w PowerShell, to kwestia jednej linijki. Użyj `Test-Path` z odpowiednią ścieżką, oto jak:

```PowerShell
# Sprawdza, czy katalog istnieje
$ścieżka = "C:\PewnyKatalog"
if (Test-Path $ścieżka) {
    Write-Host "Katalog istnieje."
} else {
    Write-Host "Katalog nie istnieje."
}
```

### Przykład wyjściowy:
Jeżeli katalog istnieje:
``` 
Katalog istnieje.
```
W przeciwnym przypadku:
```
Katalog nie istnieje.
```

## Głębsze spojrzenie:
Historia:
`Test-Path` jest poleceniem dostępnym w PowerShell od jego wczesnych wersji. Jego cel pozostaje niezmieniony - zapewnić proste narzędzie do sprawdzania istnienia elementów w systemie plików.

Alternatywy:
Wcześniejsze skrypty mogły korzystać z cmdletów takich jak `Get-Item` lub `Get-ChildItem`, ale `Test-Path` jest szybszy i bardziej czytelny dla tego konkretnego przypadku użycia.

Szczegóły implementacyjne:
`Test-Path` nie tylko sprawdza folder, ale także pliki i inne typy elementów w systemie plików. Może też badać rejestr Windows za pomocą różnych przełączników, co czyni go wszechstronnym narzędziem na różne sytuacje.

## Zobacz również:
- [Oficjalna dokumentacja Test-Path](https://docs.microsoft.com/powershell/module/microsoft.powershell.management/test-path)
- [PowerShell Gallery Script Samples](https://www.powershellgallery.com/)
- [Poradnik korzystania z Test-Path na poziomie zaawansowanym](https://ss64.com/ps/test-path.html)
